;;; task-conductor-dev-env.el --- Emacs dev-env for task-conductor -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: task-conductor contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cider "1.14.0") (claude-code "0.1.0"))
;; Keywords: tools, processes
;; URL: https://github.com/hugoduncan/task-conductor

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package registers Emacs as a dev-env with the task-conductor
;; orchestrator via nREPL.  It enables the orchestrator to delegate
;; interactive Claude sessions to Emacs, using claude-code.el as the
;; terminal backend.
;;
;; Usage:
;;   1. Start the task-conductor JVM with nREPL
;;   2. Connect to nREPL with M-x cider-connect
;;   3. Run M-x task-conductor-dev-env-connect
;;
;; The package polls the JVM for commands and dispatches them to
;; claude-code.el for execution.  Hook events (idle, close) are
;; sent back to the orchestrator.

;;; Code:

(require 'cl-lib)
(require 'cider)
(require 'claude-code)

(defgroup task-conductor-dev-env nil
  "Emacs dev-env for task-conductor."
  :group 'tools
  :prefix "task-conductor-")

(defcustom task-conductor-nrepl-host "localhost"
  "Host for the task-conductor nREPL server."
  :type 'string
  :group 'task-conductor-dev-env)

(defcustom task-conductor-nrepl-port nil
  "Port for the task-conductor nREPL server.
When nil, uses the current CIDER connection."
  :type '(choice (const :tag "Use CIDER connection" nil)
                 (integer :tag "Port number"))
  :group 'task-conductor-dev-env)

;;; State

(defvar task-conductor-dev-env--dev-env-id nil
  "The dev-env-id returned from task-conductor registration.
This is a UUID string used to identify this Emacs instance.")

(defvar task-conductor-dev-env--poll-timer nil
  "Timer for the command subscription loop.
Stores the timer object for cleanup on disconnect.")

(defvar task-conductor-dev-env--sessions
  (make-hash-table :test 'equal)
  "Hash table mapping session-id strings to buffer objects.
Used to track active claude-code sessions started by the orchestrator.")

(defun task-conductor-dev-env--connected-p ()
  "Return non-nil if connected to task-conductor as a dev-env."
  (and task-conductor-dev-env--dev-env-id
       (cider-connected-p)))

;;; nREPL communication

(defun task-conductor-dev-env--eval-sync (form)
  "Evaluate Clojure FORM synchronously via nREPL.
Returns the value on success, or signals an error on failure."
  (unless (cider-connected-p)
    (user-error "Not connected to CIDER. Run M-x cider-connect first"))
  (let* ((result (cider-nrepl-sync-request:eval form))
         (value (nrepl-dict-get result "value"))
         (err (nrepl-dict-get result "err"))
         (ex (nrepl-dict-get result "ex")))
    (when (or err ex)
      (error "nREPL eval error: %s" (or err ex)))
    (when value
      (read value))))

;;; Response helpers

(defun task-conductor-dev-env--send-response (command-id response)
  "Send RESPONSE for COMMAND-ID to the orchestrator.
Returns t if delivered, nil if command not found."
  (task-conductor-dev-env--eval-sync
   (format "(task-conductor.emacs-dev-env.interface/send-response-by-id %S %S '%S)"
           task-conductor-dev-env--dev-env-id
           command-id
           response)))

;;; Command handlers

(defun task-conductor-dev-env--handle-start-session (params)
  "Handle :start-session command with PARAMS.
PARAMS should contain :session-id.  Starts a claude-code session
with --resume <session-id>.  Returns response plist."
  (let ((session-id (plist-get params :session-id)))
    (unless session-id
      (cl-return-from task-conductor-dev-env--handle-start-session
        '(:status :error :message "Missing :session-id in params")))
    (let ((existing-buffer (gethash session-id task-conductor-dev-env--sessions)))
      (if (and existing-buffer (buffer-live-p existing-buffer))
          `(:status :ok :buffer-name ,(buffer-name existing-buffer))
        (condition-case err
            (let ((buffer (claude-code--start nil (list "--resume" session-id))))
              (if (and buffer (buffer-live-p buffer))
                  (progn
                    (puthash session-id buffer task-conductor-dev-env--sessions)
                    `(:status :ok :buffer-name ,(buffer-name buffer)))
                '(:status :error :message "claude-code--start returned nil or dead buffer")))
          (error
           `(:status :error :message ,(format "Failed to start session: %s"
                                              (error-message-string err)))))))))

;;; Command dispatch

(defun task-conductor-dev-env--dispatch-command (command)
  "Dispatch COMMAND to the appropriate handler.
COMMAND is a plist with :command-id, :command, and :params.
Returns the response to send back to the orchestrator."
  (let ((command-type (plist-get command :command))
        (command-id (plist-get command :command-id))
        (params (plist-get command :params)))
    (let ((response
           (pcase command-type
             (:start-session
              (task-conductor-dev-env--handle-start-session params))
             (:register-hook
              '(:error :not-implemented :message "register-hook not yet implemented"))
             (:query-transcript
              '(:error :not-implemented :message "query-transcript not yet implemented"))
             (:query-events
              '(:error :not-implemented :message "query-events not yet implemented"))
             (:close-session
              '(:error :not-implemented :message "close-session not yet implemented"))
             (_
              `(:error :unknown-command :message ,(format "Unknown command: %s" command-type))))))
      (task-conductor-dev-env--send-response command-id response)
      response)))

;;; Poll loop

(defun task-conductor-dev-env--poll-loop ()
  "One iteration of the command subscription loop.
Polls for a command with 5s timeout, dispatches if received.
Handles disconnection gracefully by stopping the loop."
  (condition-case err
      (when (task-conductor-dev-env--connected-p)
        (let ((result (task-conductor-dev-env--eval-sync
                       (format "(task-conductor.emacs-dev-env.interface/await-command-by-id %S 5000)"
                               task-conductor-dev-env--dev-env-id))))
          (pcase (plist-get result :status)
            (:ok
             (let ((command (plist-get result :command)))
               (task-conductor-dev-env--dispatch-command command)))
            (:timeout
             nil)  ; Normal, just wait for next poll
            (:closed
             (message "task-conductor: Command channel closed, stopping poll loop")
             (task-conductor-dev-env--stop-poll-loop)
             (setq task-conductor-dev-env--dev-env-id nil))
            (:error
             (message "task-conductor: Poll error: %s" (plist-get result :message))
             (task-conductor-dev-env--stop-poll-loop)
             (setq task-conductor-dev-env--dev-env-id nil)))))
    (error
     (message "task-conductor: nREPL error in poll loop: %s" (error-message-string err))
     (task-conductor-dev-env--stop-poll-loop)
     (setq task-conductor-dev-env--dev-env-id nil))))

(defun task-conductor-dev-env--start-poll-loop ()
  "Start the command subscription poll loop."
  (unless task-conductor-dev-env--poll-timer
    (setq task-conductor-dev-env--poll-timer
          (run-with-timer 0.1 0.1 #'task-conductor-dev-env--poll-loop))
    (message "task-conductor: Poll loop started")))

(defun task-conductor-dev-env--stop-poll-loop ()
  "Stop the command subscription poll loop."
  (when task-conductor-dev-env--poll-timer
    (cancel-timer task-conductor-dev-env--poll-timer)
    (setq task-conductor-dev-env--poll-timer nil)
    (message "task-conductor: Poll loop stopped")))

;;; Connection management

;;;###autoload
(defun task-conductor-dev-env-connect ()
  "Connect to task-conductor and register as a dev-env.
Requires an active CIDER connection.  Stores the dev-env-id for
subsequent operations and starts the command subscription loop."
  (interactive)
  (when (task-conductor-dev-env--connected-p)
    (user-error "Already connected as dev-env %s" task-conductor-dev-env--dev-env-id))
  (let ((dev-env-id (task-conductor-dev-env--eval-sync
                     "(task-conductor.emacs-dev-env.interface/register-emacs-dev-env)")))
    (setq task-conductor-dev-env--dev-env-id dev-env-id)
    (task-conductor-dev-env--start-poll-loop)
    (message "Connected to task-conductor as dev-env: %s" dev-env-id)))

(defun task-conductor-dev-env-disconnect ()
  "Disconnect from task-conductor and unregister this dev-env.
Stops the command subscription loop before unregistering."
  (interactive)
  (unless (task-conductor-dev-env--connected-p)
    (user-error "Not connected to task-conductor"))
  (task-conductor-dev-env--stop-poll-loop)
  (let ((dev-env-id task-conductor-dev-env--dev-env-id))
    (task-conductor-dev-env--eval-sync
     (format "(task-conductor.emacs-dev-env.interface/unregister-emacs-dev-env %S)"
             dev-env-id))
    (setq task-conductor-dev-env--dev-env-id nil)
    (message "Disconnected from task-conductor")))

(provide 'task-conductor-dev-env)
;;; task-conductor-dev-env.el ends here
