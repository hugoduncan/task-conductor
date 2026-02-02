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
(require 'claude-code)
(require 'parseedn)

(declare-function cider-connected-p "cider-connection")
(declare-function cider-nrepl-sync-request:eval "cider-client")
(declare-function cider-nrepl-request:eval "cider-client")
(declare-function nrepl-dict-get "nrepl-dict")

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

(defcustom task-conductor-dev-env-transcript-limit (* 100 1024)
  "Maximum size in bytes for transcript responses.
When buffer content exceeds this limit, only the last N bytes are returned."
  :type 'integer
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

(defvar task-conductor-dev-env--session-hooks
  (make-hash-table :test 'equal)
  "Hash table mapping session-id to hook data plist.
Each entry is (:on-idle (:hook-id X :timer Y) :on-close (:hook-id X :fn Y)).")

(defvar task-conductor-dev-env--project-dir nil
  "Project directory for CIDER session context.
Set during connect to ensure poll loop uses correct nREPL session.")

(defvar task-conductor-dev-env--polling nil
  "Non-nil when an async poll request is in flight.
Prevents overlapping poll requests.")

(defconst task-conductor-dev-env--idle-debounce-seconds 0.5
  "Debounce interval for idle detection in seconds.")

(defun task-conductor-dev-env--connected-p ()
  "Return non-nil if connected to task-conductor as a dev-env."
  (and task-conductor-dev-env--dev-env-id
       (fboundp 'cider-connected-p)
       (cider-connected-p)))

;;; UUID generation

(defun task-conductor-dev-env--generate-uuid ()
  "Generate a UUID v4 string.
Format: xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx where x is random hex
and y is one of 8, 9, a, or b."
  (format "%04x%04x-%04x-4%03x-%x%03x-%04x%04x%04x"
          (random 65536)
          (random 65536)
          (random 65536)
          (random 4096)
          (+ 8 (random 4))  ; y = 8, 9, a, or b
          (random 4096)
          (random 65536)
          (random 65536)
          (random 65536)))

;;; EDN conversion

(defun task-conductor-dev-env--edn-to-plist (edn)
  "Convert parsed EDN hash-table to plist recursively.
Handles special parseedn types like (edn-uuid \"...\")."
  (cond
   ((hash-table-p edn)
    (let ((result nil))
      (maphash (lambda (k v)
                 (setq result (plist-put result k (task-conductor-dev-env--edn-to-plist v))))
               edn)
      result))
   ;; Handle parseedn UUID representation: (edn-uuid "uuid-string")
   ((and (listp edn)
         (eq (car edn) 'edn-uuid)
         (stringp (cadr edn)))
    (cadr edn))
   ((listp edn)
    (mapcar #'task-conductor-dev-env--edn-to-plist edn))
   (t edn)))

(defun task-conductor-dev-env--plist-to-edn (plist)
  "Convert elisp PLIST to EDN string for Clojure."
  (let ((pairs nil))
    (while plist
      (let ((key (car plist))
            (val (cadr plist)))
        (push (format "%s %s"
                      key
                      (cond
                       ((keywordp val) (symbol-name val))
                       ((stringp val) (format "%S" val))
                       ((null val) "nil")
                       ((eq val t) "true")
                       (t (format "%s" val))))
              pairs))
      (setq plist (cddr plist)))
    (format "{%s}" (string-join (reverse pairs) " "))))

;;; nREPL communication

(defun task-conductor-dev-env--eval-sync (form)
  "Evaluate Clojure FORM synchronously via nREPL.
Returns the parsed value as plist on success, or signals an error on failure.
Uses `task-conductor-dev-env--project-dir' for CIDER session context."
  (let ((default-directory (or task-conductor-dev-env--project-dir default-directory)))
    (unless (cider-connected-p)
      (user-error "Not connected to CIDER. Run M-x cider-connect first"))
    (let* ((result (cider-nrepl-sync-request:eval form))
           (value (nrepl-dict-get result "value"))
           (err (nrepl-dict-get result "err"))
           (ex (nrepl-dict-get result "ex")))
      (when (or err ex)
        (error "nREPL eval error for %s: %s" form (or err ex)))
      (when value
        (task-conductor-dev-env--edn-to-plist (parseedn-read-str value))))))

;;; Response helpers

(defun task-conductor-dev-env--send-response (command-id response)
  "Send RESPONSE for COMMAND-ID to the orchestrator.
COMMAND-ID is a UUID string.  RESPONSE is a plist.
Returns t if delivered, nil if command not found."
  (let ((default-directory (or task-conductor-dev-env--project-dir default-directory)))
    (task-conductor-dev-env--eval-sync
     (format "(task-conductor.emacs-dev-env.interface/send-response-by-id %S #uuid %S %s)"
             task-conductor-dev-env--dev-env-id
             command-id
             (task-conductor-dev-env--plist-to-edn response)))))

(defun task-conductor-dev-env--send-hook-event (hook-type session-id reason)
  "Send hook event to the orchestrator.
HOOK-TYPE is :on-idle or :on-close.
SESSION-ID identifies the session.
REASON is :idle, :user-exit, etc."
  (condition-case err
      (task-conductor-dev-env--eval-sync
       (format "(task-conductor.emacs-dev-env.interface/send-hook-event-by-id %S %s %S %s)"
               task-conductor-dev-env--dev-env-id
               hook-type
               session-id
               reason))
    (error
     (message "task-conductor: Failed to send hook event: %s" (error-message-string err)))))

;;; Hook registration

(defun task-conductor-dev-env--find-session-for-buffer (buffer)
  "Find the session-id for BUFFER in our sessions table.
Returns session-id string or nil if not found."
  (let ((result nil))
    (maphash (lambda (session-id session-buffer)
               (when (eq session-buffer buffer)
                 (setq result session-id)))
             task-conductor-dev-env--sessions)
    result))

(defun task-conductor-dev-env--on-idle-debounced (session-id)
  "Called when bell detected in SESSION-ID's buffer, with debounce.
Cancels any pending timer and starts a new one."
  (let* ((hooks (gethash session-id task-conductor-dev-env--session-hooks))
         (idle-data (plist-get hooks :on-idle))
         (existing-timer (plist-get idle-data :timer)))
    (when existing-timer
      (cancel-timer existing-timer))
    (let ((new-timer
           (run-with-timer
            task-conductor-dev-env--idle-debounce-seconds nil
            (lambda ()
              (task-conductor-dev-env--send-hook-event :on-idle session-id :idle)))))
      (puthash session-id
               (plist-put hooks :on-idle
                          (plist-put idle-data :timer new-timer))
               task-conductor-dev-env--session-hooks))))

(defun task-conductor-dev-env--bell-advice (orig-fun &rest args)
  "Advice around `claude-code--notify' to detect idle in our sessions.
ORIG-FUN is the original function, ARGS are its arguments."
  (let ((session-id (task-conductor-dev-env--find-session-for-buffer (current-buffer))))
    (when session-id
      (let* ((hooks (gethash session-id task-conductor-dev-env--session-hooks))
             (idle-data (plist-get hooks :on-idle)))
        (when idle-data
          (task-conductor-dev-env--on-idle-debounced session-id)))))
  (apply orig-fun args))

(defun task-conductor-dev-env--setup-on-idle-hook (session-id hook-id)
  "Set up :on-idle hook for SESSION-ID with HOOK-ID.
Returns t on success."
  (let ((hooks (or (gethash session-id task-conductor-dev-env--session-hooks)
                   (puthash session-id '() task-conductor-dev-env--session-hooks))))
    ;; Cancel any existing timer for this session
    (let* ((existing (plist-get hooks :on-idle))
           (existing-timer (plist-get existing :timer)))
      (when existing-timer
        (cancel-timer existing-timer)))
    ;; Store hook data
    (puthash session-id
             (plist-put hooks :on-idle (list :hook-id hook-id :timer nil))
             task-conductor-dev-env--session-hooks)
    ;; Add advice to claude-code--notify if not already added
    (unless (advice-member-p #'task-conductor-dev-env--bell-advice 'claude-code--notify)
      (advice-add 'claude-code--notify :around #'task-conductor-dev-env--bell-advice))
    t))

(defun task-conductor-dev-env--on-close-handler ()
  "Handler for kill-buffer-hook to detect session close."
  (let ((session-id (task-conductor-dev-env--find-session-for-buffer (current-buffer))))
    (when session-id
      (let* ((hooks (gethash session-id task-conductor-dev-env--session-hooks))
             (close-data (plist-get hooks :on-close)))
        (when close-data
          (task-conductor-dev-env--send-hook-event :on-close session-id :user-exit))))))

(cl-defun task-conductor-dev-env--setup-on-close-hook (session-id hook-id)
  "Set up :on-close hook for SESSION-ID with HOOK-ID.
Returns t on success."
  (let ((buffer (gethash session-id task-conductor-dev-env--sessions)))
    (unless (and buffer (buffer-live-p buffer))
      (cl-return-from task-conductor-dev-env--setup-on-close-hook nil))
    (let ((hooks (or (gethash session-id task-conductor-dev-env--session-hooks)
                     (puthash session-id '() task-conductor-dev-env--session-hooks))))
      ;; Store hook data
      (puthash session-id
               (plist-put hooks :on-close (list :hook-id hook-id))
               task-conductor-dev-env--session-hooks)
      ;; Add kill-buffer-hook in the session buffer
      (with-current-buffer buffer
        (add-hook 'kill-buffer-hook #'task-conductor-dev-env--on-close-handler nil t))
      t)))

(defun task-conductor-dev-env--cleanup-session-hooks (session-id)
  "Clean up all hooks for SESSION-ID."
  (let ((hooks (gethash session-id task-conductor-dev-env--session-hooks)))
    (when hooks
      ;; Cancel idle timer if any
      (let ((idle-timer (plist-get (plist-get hooks :on-idle) :timer)))
        (when idle-timer
          (cancel-timer idle-timer)))
      ;; Remove from hash table
      (remhash session-id task-conductor-dev-env--session-hooks)
      ;; Remove bell advice if no more sessions with on-idle hooks
      (let ((has-idle-hooks nil))
        (maphash (lambda (_k v)
                   (when (plist-get v :on-idle)
                     (setq has-idle-hooks t)))
                 task-conductor-dev-env--session-hooks)
        (unless has-idle-hooks
          (advice-remove 'claude-code--notify #'task-conductor-dev-env--bell-advice))))))

;;; Command handlers

(cl-defun task-conductor-dev-env--handle-start-session (params)
  "Handle :start-session command with PARAMS.
PARAMS should contain :session-id.  Associates an existing claude-code
buffer with the session-id.  Returns response plist.

Note: claude-code--start does not return the buffer, so we find the
most recent buffer for the project directory."
  (let ((session-id (plist-get params :session-id)))
    (unless session-id
      (cl-return-from task-conductor-dev-env--handle-start-session
        '(:status :error :message "Missing :session-id in params")))
    (let ((existing-buffer (gethash session-id task-conductor-dev-env--sessions)))
      (if (and existing-buffer (buffer-live-p existing-buffer))
          `(:status :ok :buffer-name ,(buffer-name existing-buffer))
        ;; Find existing claude buffer for project
        (let* ((dir (or task-conductor-dev-env--project-dir default-directory))
               (bufs (claude-code--find-claude-buffers-for-directory dir)))
          (if bufs
              (let ((buf (car bufs)))
                (puthash session-id buf task-conductor-dev-env--sessions)
                `(:status :ok :buffer-name ,(buffer-name buf)))
            '(:status :error :message "No claude-code buffer found for project")))))))

(cl-defun task-conductor-dev-env--handle-register-hook (params)
  "Handle :register-hook command with PARAMS.
PARAMS should contain :session-id and :hook-type (:on-idle or :on-close).
Returns response plist with :status and :hook-id on success."
  (let ((session-id (plist-get params :session-id))
        (hook-type (plist-get params :hook-type)))
    (unless session-id
      (cl-return-from task-conductor-dev-env--handle-register-hook
        '(:status :error :message "Missing :session-id")))
    (unless hook-type
      (cl-return-from task-conductor-dev-env--handle-register-hook
        '(:status :error :message "Missing :hook-type")))
    (unless (memq hook-type '(:on-idle :on-close))
      (cl-return-from task-conductor-dev-env--handle-register-hook
        `(:status :error :message ,(format "Invalid hook-type: %s" hook-type))))
    (let ((buffer (gethash session-id task-conductor-dev-env--sessions)))
      (unless (and buffer (buffer-live-p buffer))
        (cl-return-from task-conductor-dev-env--handle-register-hook
          `(:status :error :message ,(format "Session not found or buffer dead: %s" session-id))))
      (let ((hook-id (task-conductor-dev-env--generate-uuid)))
        (pcase hook-type
          (:on-idle
           (if (task-conductor-dev-env--setup-on-idle-hook session-id hook-id)
               `(:status :ok :hook-id ,hook-id)
             '(:status :error :message "Failed to setup on-idle hook")))
          (:on-close
           (if (task-conductor-dev-env--setup-on-close-hook session-id hook-id)
               `(:status :ok :hook-id ,hook-id)
             '(:status :error :message "Failed to setup on-close hook"))))))))

(defun task-conductor-dev-env--strip-ansi-codes (string)
  "Remove ANSI escape codes from STRING."
  (replace-regexp-in-string "\033\\[[0-9;]*m" "" string))

(cl-defun task-conductor-dev-env--handle-query-transcript (params)
  "Handle :query-transcript command with PARAMS.
PARAMS should contain :session-id.  Returns buffer content with ANSI codes
stripped, limited to `task-conductor-dev-env-transcript-limit' bytes."
  (let ((session-id (plist-get params :session-id)))
    (unless session-id
      (cl-return-from task-conductor-dev-env--handle-query-transcript
        '(:status :error :message "Missing :session-id")))
    (let ((buffer (gethash session-id task-conductor-dev-env--sessions)))
      (unless buffer
        (cl-return-from task-conductor-dev-env--handle-query-transcript
          `(:status :error :message ,(format "Session not found: %s" session-id))))
      (unless (buffer-live-p buffer)
        (cl-return-from task-conductor-dev-env--handle-query-transcript
          `(:status :error :message ,(format "Session buffer is dead: %s" session-id))))
      (with-current-buffer buffer
        (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
               (stripped (task-conductor-dev-env--strip-ansi-codes content))
               (transcript (if (> (length stripped) task-conductor-dev-env-transcript-limit)
                               (substring stripped (- (length stripped)
                                                      task-conductor-dev-env-transcript-limit))
                             stripped)))
          `(:status :ok :transcript ,transcript))))))

(cl-defun task-conductor-dev-env--handle-close-session (params)
  "Handle :close-session command with PARAMS.
PARAMS should contain :session-id.  Kills the buffer (triggering :on-close
hook if registered), removes from sessions table, and cleans up hooks."
  (let ((session-id (plist-get params :session-id)))
    (unless session-id
      (cl-return-from task-conductor-dev-env--handle-close-session
        '(:status :error :message "Missing :session-id")))
    (let ((buffer (gethash session-id task-conductor-dev-env--sessions)))
      (unless buffer
        (cl-return-from task-conductor-dev-env--handle-close-session
          `(:status :error :message ,(format "Session not found: %s" session-id))))
      ;; Kill buffer if still alive (triggers :on-close hook)
      (when (buffer-live-p buffer)
        (kill-buffer buffer))
      ;; Remove from sessions table
      (remhash session-id task-conductor-dev-env--sessions)
      ;; Clean up hooks
      (task-conductor-dev-env--cleanup-session-hooks session-id)
      '(:status :ok))))

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
             (:ping
              '(:status :ok))
             (:start-session
              (task-conductor-dev-env--handle-start-session params))
             (:register-hook
              (task-conductor-dev-env--handle-register-hook params))
             (:query-transcript
              (task-conductor-dev-env--handle-query-transcript params))
             (:query-events
              '(:error :not-implemented :message "query-events not yet implemented"))
             (:close-session
              (task-conductor-dev-env--handle-close-session params))
             (_
              `(:error :unknown-command :message ,(format "Unknown command: %s" command-type))))))
      (task-conductor-dev-env--send-response command-id response)
      response)))

;;; Async poll loop

(defun task-conductor-dev-env--poll-callback (response)
  "Callback for async poll RESPONSE.
Processes the response and dispatches commands.  Non-blocking."
  (setq task-conductor-dev-env--polling nil)
  (let ((value (nrepl-dict-get response "value"))
        (err (nrepl-dict-get response "err")))
    (cond
     (err
      (message "task-conductor: Poll error: %s" err))
     (value
      (let ((result (task-conductor-dev-env--edn-to-plist (parseedn-read-str value))))
        (pcase (plist-get result :status)
          (:ok
           (let ((command (plist-get result :command)))
             (task-conductor-dev-env--dispatch-command command)))
          (:timeout nil) ; Normal, wait for next poll
          (:closed
           (message "task-conductor: Channel closed, stopping poll loop")
           (task-conductor-dev-env--stop-poll-loop)
           (setq task-conductor-dev-env--dev-env-id nil))
          (:error
           (message "task-conductor: Poll error: %s" (plist-get result :message)))))))))

(defun task-conductor-dev-env--poll-loop ()
  "One iteration of the async command subscription loop.
Non-blocking - uses callback for response handling."
  (when (and task-conductor-dev-env--dev-env-id
             (not task-conductor-dev-env--polling))
    (setq task-conductor-dev-env--polling t)
    (let ((default-directory (or task-conductor-dev-env--project-dir default-directory)))
      (condition-case err
          (cider-nrepl-request:eval
           (format "(task-conductor.emacs-dev-env.interface/await-command-by-id %S 5000)"
                   task-conductor-dev-env--dev-env-id)
           #'task-conductor-dev-env--poll-callback)
        (error
         (setq task-conductor-dev-env--polling nil)
         (message "task-conductor: Async poll error: %s" (error-message-string err)))))))

(defun task-conductor-dev-env--start-poll-loop ()
  "Start the async command subscription poll loop."
  (unless task-conductor-dev-env--poll-timer
    (setq task-conductor-dev-env--polling nil)
    (setq task-conductor-dev-env--poll-timer
          (run-with-timer 0.5 0.5 #'task-conductor-dev-env--poll-loop))
    (message "task-conductor: Async poll loop started")))

(defun task-conductor-dev-env--stop-poll-loop ()
  "Stop the command subscription poll loop."
  (when task-conductor-dev-env--poll-timer
    (cancel-timer task-conductor-dev-env--poll-timer)
    (setq task-conductor-dev-env--poll-timer nil)
    (setq task-conductor-dev-env--polling nil)
    (message "task-conductor: Poll loop stopped")))

;;; Connection management

;;;###autoload
(defun task-conductor-dev-env-connect ()
  "Connect to task-conductor and register as a dev-env.
Requires an active CIDER connection.  Stores the dev-env-id for
subsequent operations and starts the command subscription loop."
  (interactive)
  (require 'cider)
  (when (task-conductor-dev-env--connected-p)
    (user-error "Already connected as dev-env %s" task-conductor-dev-env--dev-env-id))
  ;; Store project directory for CIDER session context
  (setq task-conductor-dev-env--project-dir default-directory)
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
    (setq task-conductor-dev-env--project-dir nil)
    (clrhash task-conductor-dev-env--sessions)
    (clrhash task-conductor-dev-env--session-hooks)
    (message "Disconnected from task-conductor")))

(provide 'task-conductor-dev-env)
;;; task-conductor-dev-env.el ends here
