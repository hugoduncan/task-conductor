;;; task-conductor-dev-env.el --- Dev environment integration for task-conductor -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: task-conductor Contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
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

;;; Internal Variables

(defvar task-conductor-dev-env--server-process nil
  "The server process listening for task-conductor connections.")

(defvar task-conductor-dev-env--client-process nil
  "The current client connection process.")

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
To be implemented in session management task."
  (let ((session-id (alist-get 'session-id message))
        (prompt (alist-get 'prompt message))
        (working-dir (alist-get 'working-dir message)))
    (task-conductor-dev-env--debug
     "open-session: id=%s dir=%s prompt=%s"
     session-id working-dir (and prompt (substring prompt 0 (min 50 (length prompt)))))
    ;; Stub - will be implemented in task #156
    nil))

(defun task-conductor-dev-env--handle-close-session (message)
  "Handle close-session MESSAGE.
To be implemented in session management task."
  (let ((session-id (alist-get 'session-id message)))
    (task-conductor-dev-env--debug "close-session: id=%s" session-id)
    ;; Stub - will be implemented in task #156
    nil))

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

(defun task-conductor-dev-env--server-sentinel (proc event)
  "Sentinel for the server process.
PROC is the server process, EVENT describes the state change."
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

(defun task-conductor-dev-env--make-client-filter (server-proc)
  "Create a filter function that registers client with SERVER-PROC."
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
