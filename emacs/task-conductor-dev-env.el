;;; task-conductor-dev-env.el --- Dev environment integration for task-conductor -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: task-conductor Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (parseedn "1.2.0"))
;; Keywords: tools, processes

;;; Commentary:

;; UNIX domain socket server for task-conductor dev-env integration.
;; Receives session open/close requests from task-conductor and manages
;; Claude CLI sessions via claude-code.el.
;;
;; Protocol: newline-delimited JSON messages
;;
;; Incoming (from task-conductor):
;;   {"type": "open-session", "session-id": "...", "prompt": "...", "working-dir": "..."}
;;   {"type": "close-session", "session-id": "..."}
;;
;; Outgoing (to task-conductor):
;;   {"type": "session-complete", "session-id": "...", "status": "...", "hook-status": {...}, "exit-code": N}
;;   {"type": "error", "session-id": "...", "message": "..."}

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'parseedn)

;; Forward declarations for claude-code.el
(declare-function claude-code--start "claude-code")
(declare-function claude-code--find-claude-buffers-for-directory "claude-code")
(declare-function claude-code--term-send-string "claude-code")
(declare-function claude-code--kill-buffer "claude-code")
(defvar claude-code-terminal-backend)
(defvar claude-code-event-hook)

;;; Customization

(defgroup task-conductor-dev-env nil
  "Task-conductor development environment integration."
  :group 'tools
  :prefix "task-conductor-dev-env-")

(defcustom task-conductor-dev-env-socket-path "/tmp/task-conductor-dev-env.sock"
  "Path to the UNIX domain socket for task-conductor communication."
  :type 'file
  :group 'task-conductor-dev-env)

(defcustom task-conductor-dev-env-debug nil
  "Enable debug logging for task-conductor-dev-env."
  :type 'boolean
  :group 'task-conductor-dev-env)

(defcustom task-conductor-dev-env-prompt-delay 0.5
  "Delay in seconds before sending prompt after session starts.
Allows time for the Claude CLI session to initialize."
  :type 'number
  :group 'task-conductor-dev-env)

;;; Internal Variables

(defvar task-conductor-dev-env--server-process nil
  "The server process listening for task-conductor connections.")

(defvar task-conductor-dev-env--client-process nil
  "The current client connection process.")

(defvar task-conductor-dev-env--sessions (make-hash-table :test 'equal)
  "Hash table mapping session-id to Claude buffer.")

(defvar task-conductor-dev-env--buffer-sessions (make-hash-table :test 'eq)
  "Hash table mapping buffer to session-id for reverse lookup.")

;;; Debug Logging

(defun task-conductor-dev-env--debug (format-string &rest args)
  "Log debug message using FORMAT-STRING and ARGS when debug is enabled."
  (when task-conductor-dev-env-debug
    (apply #'message (concat "task-conductor-dev-env: " format-string) args)))

;;; Message Parsing

(defun task-conductor-dev-env--parse-message (line)
  "Parse LINE as JSON and return an alist.
Returns nil if parsing fails."
  (condition-case err
      (json-read-from-string line)
    (json-error
     (task-conductor-dev-env--debug "JSON parse error: %s" (error-message-string err))
     nil)))

;;; Message Sending

(defun task-conductor-dev-env--send-message (message)
  "Send MESSAGE as JSON to the connected client.
MESSAGE should be an alist. Returns t on success, nil on failure."
  (when (and task-conductor-dev-env--client-process
             (process-live-p task-conductor-dev-env--client-process))
    (condition-case err
        (let ((json-str (concat (json-encode message) "\n")))
          (process-send-string task-conductor-dev-env--client-process json-str)
          (task-conductor-dev-env--debug "Sent: %s" (string-trim json-str))
          t)
      (error
       (task-conductor-dev-env--debug "Send error: %s" (error-message-string err))
       nil))))

(defun task-conductor-dev-env-send-session-complete (session-id status hook-status exit-code)
  "Send session-complete message for SESSION-ID.
STATUS is a string (\"completed\", \"cancelled\", \"error\").
HOOK-STATUS is an alist of hook status data.
EXIT-CODE is the CLI exit code."
  (task-conductor-dev-env--send-message
   `((type . "session-complete")
     (session-id . ,session-id)
     (status . ,status)
     (hook-status . ,hook-status)
     (exit-code . ,exit-code))))

(defun task-conductor-dev-env-send-error (session-id error-message)
  "Send error message for SESSION-ID with ERROR-MESSAGE."
  (task-conductor-dev-env--send-message
   `((type . "error")
     (session-id . ,session-id)
     (message . ,error-message))))

;;; Session Management

(defun task-conductor-dev-env--register-session (session-id buffer)
  "Register SESSION-ID with BUFFER in session tracking tables."
  (puthash session-id buffer task-conductor-dev-env--sessions)
  (puthash buffer session-id task-conductor-dev-env--buffer-sessions)
  (task-conductor-dev-env--debug "Registered session %s with buffer %s"
                                  session-id (buffer-name buffer)))

(defun task-conductor-dev-env--unregister-session (session-id)
  "Remove SESSION-ID from session tracking tables."
  (when-let ((buffer (gethash session-id task-conductor-dev-env--sessions)))
    (remhash buffer task-conductor-dev-env--buffer-sessions))
  (remhash session-id task-conductor-dev-env--sessions)
  (task-conductor-dev-env--debug "Unregistered session %s" session-id))

(defun task-conductor-dev-env--get-buffer (session-id)
  "Get the buffer associated with SESSION-ID."
  (gethash session-id task-conductor-dev-env--sessions))

(defun task-conductor-dev-env--get-session-id (buffer)
  "Get the session-id associated with BUFFER."
  (gethash buffer task-conductor-dev-env--buffer-sessions))

(defun task-conductor-dev-env--edn-to-alist (edn-value)
  "Convert EDN-VALUE from parseedn to alist with symbol keys.
parseedn returns hash tables for maps and keywords for EDN keywords.
This converts to alists with plain symbol keys for compatibility."
  (cond
   ((hash-table-p edn-value)
    (let (result)
      (maphash (lambda (k v)
                 ;; Convert keyword :foo to symbol foo
                 (let ((key (if (keywordp k)
                               (intern (substring (symbol-name k) 1))
                             k)))
                   (push (cons key (task-conductor-dev-env--edn-to-alist v))
                         result)))
               edn-value)
      (nreverse result)))
   ((listp edn-value)
    (mapcar #'task-conductor-dev-env--edn-to-alist edn-value))
   ((keywordp edn-value)
    ;; Convert keyword values to strings (e.g., :completed -> "completed")
    (substring (symbol-name edn-value) 1))
   (t edn-value)))

(defun task-conductor-dev-env--read-handoff-edn (working-dir)
  "Read hook status from .task-conductor/handoff.edn in WORKING-DIR.
Returns an alist or nil if file doesn't exist.
Uses parseedn for robust EDN parsing."
  (let ((handoff-file (expand-file-name ".task-conductor/handoff.edn" working-dir)))
    (when (file-exists-p handoff-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents handoff-file)
            (goto-char (point-min))
            ;; parseedn-read returns a list; extract the first element
            (let ((parsed (car (parseedn-read))))
              (task-conductor-dev-env--edn-to-alist parsed)))
        (error
         (task-conductor-dev-env--debug "Error reading handoff.edn: %s"
                                        (error-message-string err))
         nil)))))

(defun task-conductor-dev-env--buffer-kill-handler ()
  "Handle buffer being killed - send error if session was active."
  (when-let ((session-id (task-conductor-dev-env--get-session-id (current-buffer))))
    (task-conductor-dev-env--debug "Buffer killed for session %s" session-id)
    (task-conductor-dev-env-send-error session-id "Session buffer closed by user")
    (task-conductor-dev-env--unregister-session session-id)))

;;; Event Hook Integration

(defun task-conductor-dev-env--event-hook-handler (event)
  "Handle claude-code-event-hook EVENT.
Detects idle/completion events and sends session-complete response."
  (let* ((event-type (plist-get event :type))
         (buffer-name (plist-get event :buffer-name))
         (json-data (plist-get event :json-data))
         (buffer (and buffer-name (get-buffer buffer-name)))
         (session-id (and buffer (task-conductor-dev-env--get-session-id buffer))))
    (task-conductor-dev-env--debug "Event: type=%s buffer=%s session=%s"
                                    event-type buffer-name session-id)
    ;; Only handle events for sessions we're tracking
    (when session-id
      (cond
       ;; Handle idle/stop events
       ((member event-type '("stop" "idle" "finished"))
        (let* ((working-dir (and buffer (buffer-local-value 'default-directory buffer)))
               (hook-status (or (task-conductor-dev-env--read-handoff-edn working-dir)
                               json-data
                               '()))
               (exit-code (or (and json-data (alist-get 'exit-code json-data))
                             (and (listp hook-status) (alist-get 'exit-code hook-status))
                             0)))
          (task-conductor-dev-env--debug
           "Session %s completed with status %s" session-id event-type)
          (task-conductor-dev-env-send-session-complete
           session-id
           (if (equal event-type "stop") "completed" event-type)
           hook-status
           exit-code)
          ;; Unregister session after completion
          (task-conductor-dev-env--unregister-session session-id)))
       ;; Handle error events
       ((equal event-type "error")
        (let ((error-msg (or (and json-data (alist-get 'message json-data))
                            "Unknown error")))
          (task-conductor-dev-env-send-error session-id error-msg)
          (task-conductor-dev-env--unregister-session session-id)))))))

(defun task-conductor-dev-env--setup-event-hook ()
  "Add task-conductor handler to claude-code-event-hook."
  (when (boundp 'claude-code-event-hook)
    (add-hook 'claude-code-event-hook #'task-conductor-dev-env--event-hook-handler)))

(defun task-conductor-dev-env--teardown-event-hook ()
  "Remove task-conductor handler from claude-code-event-hook."
  (when (boundp 'claude-code-event-hook)
    (remove-hook 'claude-code-event-hook #'task-conductor-dev-env--event-hook-handler)))

;;; Message Dispatch

(defvar task-conductor-dev-env-message-handlers
  '(("open-session" . task-conductor-dev-env--handle-open-session)
    ("close-session" . task-conductor-dev-env--handle-close-session))
  "Alist mapping message types to handler functions.")

(defun task-conductor-dev-env--dispatch-message (message)
  "Dispatch MESSAGE to the appropriate handler based on type field."
  (let* ((msg-type (alist-get 'type message))
         (handler (alist-get msg-type task-conductor-dev-env-message-handlers
                             nil nil #'string=)))
    (if handler
        (funcall handler message)
      (task-conductor-dev-env--debug "Unknown message type: %s" msg-type))))

(defun task-conductor-dev-env--handle-open-session (message)
  "Handle open-session MESSAGE.
Starts a Claude CLI session with --resume using claude-code.el."
  (let ((session-id (alist-get 'session-id message))
        (prompt (alist-get 'prompt message))
        (working-dir (alist-get 'working-dir message)))
    (task-conductor-dev-env--debug
     "open-session: id=%s dir=%s prompt=%s"
     session-id working-dir (and prompt (substring prompt 0 (min 50 (length prompt)))))
    (condition-case err
        (let* ((default-directory (or working-dir default-directory))
               ;; Start Claude with --resume <session-id>
               (extra-switches (list "--resume" session-id))
               ;; Override claude-code--directory to use working-dir
               (buffer (cl-letf (((symbol-function 'claude-code--directory)
                                  (lambda () default-directory)))
                         ;; Start Claude without prompting for instance name
                         ;; and don't switch to buffer automatically
                         (claude-code--start nil extra-switches nil nil)
                         ;; Get the buffer that was just created
                         (car (claude-code--find-claude-buffers-for-directory
                               default-directory)))))
          (if buffer
              (progn
                ;; Register the session
                (task-conductor-dev-env--register-session session-id buffer)
                ;; Add buffer kill handler
                (with-current-buffer buffer
                  (add-hook 'kill-buffer-hook
                            #'task-conductor-dev-env--buffer-kill-handler nil t))
                ;; Send prompt if provided, after a small delay for session to initialize
                (when (and prompt (not (string-empty-p prompt)))
                  (run-at-time task-conductor-dev-env-prompt-delay nil
                               (lambda (buf prompt-text)
                                 (when (buffer-live-p buf)
                                   (with-current-buffer buf
                                     (claude-code--term-send-string
                                      claude-code-terminal-backend prompt-text)
                                     (sit-for 0.1)
                                     (claude-code--term-send-string
                                      claude-code-terminal-backend (kbd "RET")))))
                               buffer prompt))
                (task-conductor-dev-env--debug "Session %s started in buffer %s"
                                               session-id (buffer-name buffer)))
            (task-conductor-dev-env-send-error session-id "Failed to create Claude buffer")))
      (error
       (task-conductor-dev-env--debug "Error starting session: %s"
                                      (error-message-string err))
       (task-conductor-dev-env-send-error session-id
                                          (format "Error: %s" (error-message-string err)))))))

(defun task-conductor-dev-env--handle-close-session (message)
  "Handle close-session MESSAGE.
Closes the Claude CLI session buffer."
  (let ((session-id (alist-get 'session-id message)))
    (task-conductor-dev-env--debug "close-session: id=%s" session-id)
    (if-let ((buffer (task-conductor-dev-env--get-buffer session-id)))
        (progn
          ;; Remove session first to prevent kill-handler from sending error
          (task-conductor-dev-env--unregister-session session-id)
          ;; Kill the buffer
          (when (buffer-live-p buffer)
            (with-current-buffer buffer
              ;; Remove our kill handler first
              (remove-hook 'kill-buffer-hook
                           #'task-conductor-dev-env--buffer-kill-handler t)
              ;; Kill the Claude process and buffer
              (claude-code--kill-buffer buffer)))
          (task-conductor-dev-env--debug "Session %s closed" session-id))
      (task-conductor-dev-env--debug "Session %s not found" session-id))))

;;; Process Filter and Sentinel

(defun task-conductor-dev-env--filter (proc string)
  "Process filter for task-conductor client connection.
PROC is the connection process, STRING is received data.
Buffers incomplete lines and dispatches complete JSON messages."
  (if (string-empty-p string)
      ;; EOF - close connection
      (delete-process proc)
    (let* ((buffer (or (process-get proc 'line-buffer) ""))
           (combined (concat buffer string))
           (lines (split-string combined "\n")))
      ;; Process all complete lines (all but the last fragment)
      (let ((complete-lines (butlast lines))
            (last-fragment (car (last lines))))
        (dolist (line complete-lines)
          (unless (string-empty-p line)
            (task-conductor-dev-env--debug "Received: %s" line)
            (when-let ((message (task-conductor-dev-env--parse-message line)))
              (task-conductor-dev-env--dispatch-message message))))
        ;; Store incomplete fragment
        (process-put proc 'line-buffer last-fragment)))))

(defun task-conductor-dev-env--sentinel (proc event)
  "Process sentinel for task-conductor connections.
PROC is the connection process, EVENT describes the state change."
  (let ((event-desc (string-trim event)))
    (task-conductor-dev-env--debug "Connection event: %s" event-desc)
    ;; Process any remaining buffered data
    (when-let ((remaining (process-get proc 'line-buffer)))
      (unless (string-empty-p remaining)
        (when-let ((message (task-conductor-dev-env--parse-message remaining)))
          (task-conductor-dev-env--dispatch-message message))))
    ;; Clean up
    (process-put proc 'line-buffer nil)
    (when (eq proc task-conductor-dev-env--client-process)
      (setq task-conductor-dev-env--client-process nil))))

(defun task-conductor-dev-env--server-sentinel (_proc event)
  "Sentinel for the server process.
_PROC is the server process, EVENT describes the state change."
  (task-conductor-dev-env--debug "Server event: %s" (string-trim event)))

;;; Socket Management

(defun task-conductor-dev-env--cleanup-socket ()
  "Remove the socket file if it exists."
  (when (file-exists-p task-conductor-dev-env-socket-path)
    (condition-case err
        (progn
          (delete-file task-conductor-dev-env-socket-path)
          (task-conductor-dev-env--debug "Cleaned up socket at %s"
                                         task-conductor-dev-env-socket-path))
      (file-error
       (message "task-conductor-dev-env: Failed to delete socket: %s"
                (error-message-string err))))))

(defun task-conductor-dev-env--make-client-filter (_server-proc)
  "Create a filter function that registers client with _SERVER-PROC."
  (lambda (proc string)
    ;; Register as current client on first data
    (unless task-conductor-dev-env--client-process
      (setq task-conductor-dev-env--client-process proc)
      (task-conductor-dev-env--debug "Client connected"))
    (task-conductor-dev-env--filter proc string)))

;;; Public API

;;;###autoload
(defun task-conductor-dev-env-start ()
  "Start the task-conductor dev-env socket server.
Creates a UNIX domain socket at `task-conductor-dev-env-socket-path'."
  (interactive)
  ;; Stop existing server if running
  (when task-conductor-dev-env--server-process
    (task-conductor-dev-env-stop))
  ;; Cleanup stale socket
  (task-conductor-dev-env--cleanup-socket)
  ;; Create server
  (condition-case err
      (progn
        (setq task-conductor-dev-env--server-process
              (make-network-process
               :name "task-conductor-dev-env-server"
               :family 'local
               :service task-conductor-dev-env-socket-path
               :server t
               :filter (task-conductor-dev-env--make-client-filter nil)
               :sentinel #'task-conductor-dev-env--sentinel
               :coding 'utf-8-unix))
        (set-process-sentinel task-conductor-dev-env--server-process
                              #'task-conductor-dev-env--server-sentinel)
        ;; Setup event hook integration
        (task-conductor-dev-env--setup-event-hook)
        (message "task-conductor-dev-env: Server started on %s"
                 task-conductor-dev-env-socket-path))
    (file-error
     (message "task-conductor-dev-env: Failed to create socket: %s"
              (error-message-string err)))
    (error
     (message "task-conductor-dev-env: Unexpected error: %s"
              (error-message-string err)))))

;;;###autoload
(defun task-conductor-dev-env-stop ()
  "Stop the task-conductor dev-env socket server."
  (interactive)
  ;; Teardown event hook integration
  (task-conductor-dev-env--teardown-event-hook)
  ;; Close client connection if active
  (when (and task-conductor-dev-env--client-process
             (process-live-p task-conductor-dev-env--client-process))
    (delete-process task-conductor-dev-env--client-process)
    (setq task-conductor-dev-env--client-process nil))
  ;; Stop server
  (when task-conductor-dev-env--server-process
    (delete-process task-conductor-dev-env--server-process)
    (setq task-conductor-dev-env--server-process nil)
    (message "task-conductor-dev-env: Server stopped"))
  ;; Clear session tracking
  (clrhash task-conductor-dev-env--sessions)
  (clrhash task-conductor-dev-env--buffer-sessions)
  ;; Cleanup socket file
  (task-conductor-dev-env--cleanup-socket))

(defun task-conductor-dev-env-running-p ()
  "Return non-nil if the server is running."
  (and task-conductor-dev-env--server-process
       (process-live-p task-conductor-dev-env--server-process)))

(defun task-conductor-dev-env-connected-p ()
  "Return non-nil if a client is connected."
  (and task-conductor-dev-env--client-process
       (process-live-p task-conductor-dev-env--client-process)))

(provide 'task-conductor-dev-env)
;;; task-conductor-dev-env.el ends here
