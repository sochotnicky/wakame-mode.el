;;; wakame-mode.el --- wakatime mode in elisp -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 Stanislav Ochotnický
;;
;; Author: Stanislav Ochotnický <stanislav@ochotnicky.com>
;; Maintainer: Stanislav Ochotnický <stanislav@ochotnicky.com>
;; Created: April 17, 2023
;; Modified: April 17, 2023
;; Version: 0.0.1
;; Keywords: convenience data docs files languages lisp  tools
;; Homepage: https://github.com/sochotnicky/wakame-mode
;; Package-Requires: ((emacs "28.2"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Enable collection of activity for WakaTime using native Emacs functionality.
;;  Unlike official wakatime-mode this way we can provide support for more
;;  built-in functionality:
;;  - Tramp
;;  - Project.el and projectile project detection
;;  - Non-file buffers (dired, proced etc.)
;;
;;
;;  Description
;;
;;; Code:

(require 'magit-git)
(require 'project)
(require 'json)
(require 'auth-source)
(require 'url-parse)

(defconst wakame-mode-version "0.0.1")
(defconst wakame-mode-user-agent "emacs-wakatime")


(defgroup wakame nil
  "Customizations for WakaTime native mode."
  :group 'convenience
  :prefix "wakame")

(defcustom wakame-api-url "https://wakapi.dev/api"
  "API URL for WakaTime."
  :type 'string
  :group 'wakame)

(defcustom wakame-cache-file (file-name-concat (or (getenv "XDG_CACHE_HOME") "~/.cache/") "wakame-mode.cache")
  "File path used for caching WakaTime heartbeats when offline."
  :type 'string
  :group 'wakame)

(defcustom wakame-secret-retry-delay 300
  "Seconds to wait before retrying a failed or cancelled secret lookup."
  :type 'integer
  :group 'wakame)

(defcustom wakame-secret-lookup-idle-delay 1
  "Seconds of idle time after a command before looking up the API secret."
  :type 'number
  :group 'wakame)

(defvar wakame--secret '()
  "Secret for given wakame server from auth-source")

(defvar wakame--secret-failed-at nil
  "Time of the last failed secret lookup, or nil.")

(defvar wakame--secret-lookup-pending nil
  "Non-nil while a secret lookup is scheduled.")

(defvar wakame--heartbeats '()
  "List of heartbeats yet to be sent to wakatime.")

(defvar wakame--idle-timer nil
  "Idle timer for posting heartbeats")

(defun wakame--pretty-mode()
  "Return nice human-readable type of current file/buffer."
  (if (buffer-file-name)
      (if (listp mode-name)
          (car mode-name)
        mode-name)))

(defvar wakame--last-heartbeat-entity nil
  "Value of last entity that was added to the heartbeats.")

(defvar wakame--last-heartbeat-time nil
  "Time of of last heartbeat was added.")

(defun wakame--current-type()
  "Return whether current buffer is file backed or considered an app."
  (if (buffer-file-name)
      "file"
    "app"))

(defun wakame--current-entity()
  "Return path of current entity or app name - i.e. major mode."
  (or (buffer-file-name)
      (symbol-name major-mode)))

(defun wakame--git-branch-name ()
  "Get current git branch name."
  (let ((args '("symbolic-ref" "HEAD" "--short")))
    (with-temp-buffer
      (apply #'process-file "git" nil (list t nil) nil args)
      (unless (bobp)
        (goto-char (point-min))
        (buffer-substring-no-properties (point) (line-end-position))))))

(defun wakame--vc-info-available-p ()
  "Return non-nil if git and project info can be read for `default-directory'.

Local directories always qualify.  Remote ones only when their Tramp
connection is already established: heartbeats are created from a
timer, which can fire while Tramp itself is still connecting or
waiting for output (redisplay runs the hooks that queue heartbeats,
and Tramp lets timers created during its waits run).  Opening or
reusing that connection from the timer then fails with \"Forbidden
reentrant call of Tramp\" and can break the connection being set up."
  (or (not (file-remote-p default-directory))
      (file-remote-p default-directory nil 'connected)))

(defun wakame--create-heartbeat(savep &optional time)
  "Return a new heartbeat.

If SAVEP is non-nil record writing heartbeat.  TIME defaults to now.

Branch and project are only looked up when that does not open a
remote connection (see `wakame--vc-info-available-p'), with
`non-essential' bound so Tramp never prompts or connects, and
errors (e.g. a busy Tramp connection) just leave them empty."
  (let* ((non-essential t)
         (vc-info (wakame--vc-info-available-p))
         (branch (and vc-info
                      (condition-case nil (wakame--git-branch-name)
                        (error nil))))
         (project (and vc-info
                       (condition-case nil (project-current)
                         (error nil)))))
    (wakame--make-heartbeat savep time branch
                            (if project (project-name project) ""))))

(defun wakame--make-heartbeat(savep time branch project)
  "Return a heartbeat with SAVEP, TIME, BRANCH and PROJECT."
  `(
    (type . ,(wakame--current-type))
    (time . ,(or time (float-time)))
    (branch . ,branch)
    (project . ,project)
    (language . ,(wakame--pretty-mode))
    (is_write . ,savep)
    ;; See ParseUserAgent at https://github.com/muety/wakapi/blob/master/utils/http.go
    (user_agent . ,(format "wakatime/unset (linux-unset) Emacs emacs-wakatime/1.0"))
    (entity . ,(wakame--current-entity))))


(defconst wakame--non-user-commands
  '(handle-switch-frame handle-focus-in handle-focus-out
    handle-select-window handle-delete-frame handle-move-frame ignore)
  "Commands run for window system events rather than user input.")

(defun wakame--lookup-after-command ()
  "Schedule the secret lookup once a command typed by the user ran.

Frame creation (e.g. a new emacsclient frame) also changes the
selected window and thus adds heartbeats, but prompting while the
frame is still being set up leaves the prompt stuck, so wait for
real input."
  (unless (memq this-command wakame--non-user-commands)
    (remove-hook 'post-command-hook #'wakame--lookup-after-command)
    (run-with-idle-timer wakame-secret-lookup-idle-delay nil #'wakame--lookup-secret)))

(defun wakame--lookup-secret ()
  "Look up the API secret via auth-source.

This may decrypt an encrypted auth-source file and therefore block
on a pinentry prompt, so it is only run once Emacs goes idle after a
user command (see `wakame--maybe-lookup-secret'), never during init
or from the heartbeat timer.  While a minibuffer is active or the
selected frame does not have focus, the lookup waits for the next
command instead: a pinentry prompt cannot pop up its window from a
selected minibuffer, and a prompt in an unfocused frame does not get
the user's input.  Errors and quits are caught so a cancelled or
timed-out prompt only delays the next attempt by
`wakame-secret-retry-delay'."
  (if (or (active-minibuffer-window)
          (not (frame-focus-state)))
      (add-hook 'post-command-hook #'wakame--lookup-after-command)
    (setq wakame--secret-lookup-pending nil)
    (unless wakame--secret
      (condition-case err
          (setq wakame--secret
                (car (auth-source-search
                      :host (url-host (url-generic-parse-url wakame-api-url))
                      :user "wakame" :max 1)))
        ((error quit)
         (message "wakame: could not read API secret: %S" err)))
      (setq wakame--secret-failed-at (unless wakame--secret (float-time))))))

(defun wakame--maybe-lookup-secret ()
  "Schedule a secret lookup if none is known and none is pending.

The lookup runs once Emacs is idle after the next user command (see
`wakame--lookup-after-command'), never directly, so it does not block
whatever added the heartbeat."
  (unless (or wakame--secret
              wakame--secret-lookup-pending
              (and wakame--secret-failed-at
                   (< (- (float-time) wakame--secret-failed-at)
                      wakame-secret-retry-delay)))
    (setq wakame--secret-lookup-pending t)
    (add-hook 'post-command-hook #'wakame--lookup-after-command)))

(defun wakame--secret-string ()
  "Return the API secret as a string, or nil if it is not known yet."
  (when-let* ((secret (plist-get wakame--secret :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun wakame--send-all-heartbeats ()
  "Sends next batch of 25 heartbeats using bulk API endpoint.

Does nothing until the secret is known; heartbeats stay queued."
  (if-let* ((secret (wakame--secret-string))
            (url-request-method "POST")
            (current-heartbeats (last wakame--heartbeats 25))
            (url-request-data (encode-coding-string (json-encode current-heartbeats) 'utf-8))
            (url-request-extra-headers
             `(("Authorization" . ,(format "Basic %s" (base64-encode-string secret)))
               ("Content-Type" . "application/json")
               ("X-Machine-Name" . ,(system-name)))))
      (progn
        (url-retrieve
         (format "%s/heartbeats" wakame-api-url)
         (lambda(status cbargs)
           (if (plist-member status :error)
               (error "Error posting heartbeats: %s" (plist-member status :error))
             (progn
               (message "Sent %s heartbeats to wakatime." (length current-heartbeats))
               (setq wakame--heartbeats (butlast wakame--heartbeats 25))
               (wakame--send-all-heartbeats))))
         (list url-request-data)
         t
         t))))

(defun wakame--add-heartbeat(heartbeat)
  "Add HEARTBEAT to heartbeats and track last addition.

Only adds heartbeat if same entity (file/app) has not been added
to the heartbeat last in past 2 minutes"
  (let ((entity (alist-get 'entity heartbeat))
        (timediff (- (float-time) (or wakame--last-heartbeat-time 120))))
    (if (or (not (equal wakame--last-heartbeat-entity entity))
            (> timediff 120))
        (progn
          (setq wakame--last-heartbeat-time (alist-get 'time heartbeat))
          (setq wakame--last-heartbeat-entity (alist-get 'entity heartbeat))
          (add-to-list 'wakame--heartbeats heartbeat)
          (wakame--maybe-lookup-secret)))))

(defvar wakame--pending-heartbeats '()
  "Heartbeats queued by hooks, newest first.
Each element is (BUFFER SAVEP TIME).")

(defvar wakame--pending-timer nil
  "Idle timer adding the heartbeats in `wakame--pending-heartbeats'.")

(defun wakame--queue-heartbeat(savep)
  "Queue a heartbeat for the current buffer.

The heartbeat is created later by `wakame--add-pending-heartbeats'
from an idle timer, because creating it runs git via `process-file'.
Hooks can run in the middle of `call-process' (e.g. window change
functions from redisplay), where that fails with \"call-process
invoked recursively\", and in the middle of a Tramp operation, where
a nested remote call fails with \"Forbidden reentrant call of Tramp\"
and can kill the connection the outer operation is using.

A timer created here would not help with the latter: Tramp suspends
existing timers while it waits for output, but runs timers created
during that wait.  The idle timer is created when the mode is
enabled, so it is suspended as well, and idle timers only run once
the current command has finished."
  (push (list (current-buffer) savep (float-time)) wakame--pending-heartbeats))

(defun wakame--add-pending-heartbeats()
  "Add the heartbeats queued by `wakame--queue-heartbeat'."
  (let ((pending (nreverse wakame--pending-heartbeats)))
    (setq wakame--pending-heartbeats nil)
    (pcase-dolist (`(,buffer ,savep ,time) pending)
      (wakame--add-queued-heartbeat buffer savep time))))

(defun wakame--add-queued-heartbeat(buffer savep time)
  "Add a heartbeat for BUFFER with SAVEP recorded at TIME."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (wakame--add-heartbeat (wakame--create-heartbeat savep time)))))

(defun wakame--handle-save()
  "Handler for buffer save."
  (wakame--queue-heartbeat t))

(defun wakame--buffer-change(&optional frame)
  "Handler for buffer focus change."
  (wakame--queue-heartbeat nil))

(defun wakame--handle-first-change()
  "Handler for first modification of a buffer.

Internal buffers (name starting with a space) are skipped.  These
include the temporary buffers `call-process' and git (see
`wakame--git-branch-name') insert output into."
  (unless (string-prefix-p " " (buffer-name))
    (wakame--buffer-change)))

(defun wakame-mode--enable()
  "Add hooks to enable wakame tracking.

The API secret is not read here; see `wakame--lookup-secret'."
  (setq wakame--secret nil
        wakame--secret-failed-at nil
        wakame--secret-lookup-pending nil)
  (setq wakame--idle-timer (run-with-idle-timer 10 t (lambda() (wakame--send-all-heartbeats))))
  (setq wakame--pending-timer (run-with-idle-timer 0.5 t #'wakame--add-pending-heartbeats))
  (add-to-list 'window-selection-change-functions #'wakame--buffer-change)
  (add-hook 'first-change-hook #'wakame--handle-first-change)
  (add-hook 'after-save-hook #'wakame--handle-save))

(defun wakame-mode--disable()
  "Remove hooks and disable wakame tracking."
  (if (timerp wakame--idle-timer) (cancel-timer wakame--idle-timer))
  (if (timerp wakame--pending-timer) (cancel-timer wakame--pending-timer))
  (setq wakame--pending-heartbeats nil)
  (setq window-selection-change-functions (delete #'wakame--buffer-change window-selection-change-functions))
  (remove-hook 'after-save-hook #'wakame--handle-save)
  (remove-hook 'first-change-hook #'wakame--handle-first-change)
  (remove-hook 'post-command-hook #'wakame--lookup-after-command))

;;;###autoload
(define-minor-mode wakame-mode
  "Toggle WakaMeMode (WakaTime Native mode)."
  :init-value nil
  :global     t
  :group      'wakame
  (cond
   (noninteractive (setq wakame-mode nil))
   (wakame-mode (wakame-mode--enable))
   (t (wakame-mode--disable))))

(provide 'wakame-mode)
;;; wakame-mode.el ends here
