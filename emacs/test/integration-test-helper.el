;;; integration-test-helper.el --- Helper for end-to-end integration tests -*- lexical-binding: t; -*-

;;; Commentary:

;; This script runs Emacs in batch mode for integration testing.
;; It starts the task-conductor-dev-env socket server and mocks
;; claude-code.el functions to allow testing without a real Claude CLI.
;;
;; Usage:
;;   emacs --batch -L /path/to/emacs -l integration-test-helper.el \
;;         -f integration-test-run
;;
;; Protocol:
;; - Prints "READY" to stdout when socket server is listening
;; - Prints "SESSION:<session-id>" when a session is opened
;; - Prints "COMPLETE:<session-id>" when session-complete is sent
;; - Prints "ERROR:<message>" on errors
;; - Exits when stdin is closed or after timeout

;;; Code:

(require 'task-conductor-dev-env)

;;; Configuration

(defvar integration-test-socket-path
  (or (getenv "TEST_SOCKET_PATH")
      "/tmp/task-conductor-integration-test.sock")
  "Socket path for integration tests.")

(defvar integration-test-session-delay 0.1
  "Delay in seconds before sending session-complete response.")

(defvar integration-test-timeout 30
  "Maximum runtime in seconds before auto-exit.")

;;; Mock session buffers

(defvar integration-test--mock-buffers (make-hash-table :test 'equal)
  "Hash table mapping session-id to mock buffer.")

;;; Mock claude-code functions

(defun integration-test--mock-claude-code-start (_instance-name extra-switches &rest _args)
  "Mock claude-code--start that creates a fake session buffer.
EXTRA-SWITCHES should contain --resume <session-id>."
  (let* ((session-id (cadr (member "--resume" extra-switches)))
         (buf-name (format "*claude:mock-%s*" session-id))
         (buf (generate-new-buffer buf-name)))
    (with-current-buffer buf
      (setq default-directory (or default-directory "/")))
    (puthash session-id buf integration-test--mock-buffers)
    (message "integration-test: Created mock buffer %s for session %s" buf-name session-id)
    buf))

(defun integration-test--mock-find-buffers-for-directory (_dir)
  "Mock claude-code--find-claude-buffers-for-directory."
  ;; Return the most recently created mock buffer
  (let (result)
    (maphash (lambda (_k v) (setq result v)) integration-test--mock-buffers)
    (when result (list result))))

(defun integration-test--mock-term-send-string (_backend _string)
  "Mock claude-code--term-send-string - no-op in tests.")

(defun integration-test--mock-kill-buffer (buf)
  "Mock claude-code--kill-buffer."
  (when (buffer-live-p buf)
    (kill-buffer buf)))

;;; Install mocks

(defun integration-test--install-mocks ()
  "Install mock functions for claude-code.el."
  (fset 'claude-code--start #'integration-test--mock-claude-code-start)
  (fset 'claude-code--find-claude-buffers-for-directory
        #'integration-test--mock-find-buffers-for-directory)
  (fset 'claude-code--term-send-string #'integration-test--mock-term-send-string)
  (fset 'claude-code--kill-buffer #'integration-test--mock-kill-buffer)
  ;; Set up variable that would normally come from claude-code.el
  (defvar claude-code-terminal-backend 'mock)
  (defvar claude-code-event-hook nil))

;;; Session completion simulation

(defun integration-test--complete-session (session-id status hook-status exit-code)
  "Simulate session completion for SESSION-ID.
STATUS is the completion status string.
HOOK-STATUS is the hook status alist.
EXIT-CODE is the exit code."
  (message "integration-test: Completing session %s with status %s" session-id status)
  (message "COMPLETE:%s" session-id)
  (task-conductor-dev-env-send-session-complete session-id status hook-status exit-code)
  ;; Clean up mock buffer
  (when-let ((buf (gethash session-id integration-test--mock-buffers)))
    (remhash session-id integration-test--mock-buffers)
    (when (buffer-live-p buf)
      (kill-buffer buf))))

;;; Custom open-session handler for testing

(defun integration-test--handle-open-session (message)
  "Handle open-session MESSAGE in test mode.
Creates a mock session and schedules completion.
Uses tracking-id for callback correlation (matching production behavior)."
  (let* ((session-id (alist-get 'session-id message))
         (tracking-id (alist-get 'tracking-id message))
         (working-dir (alist-get 'working-dir message))
         ;; Use tracking-id for callback correlation, like production code
         (correlation-id (or tracking-id session-id)))
    (message "integration-test: Received open-session for %s (tracking: %s)"
             session-id tracking-id)
    (message "SESSION:%s" session-id)
    (condition-case err
        (let* ((default-directory (or working-dir default-directory))
               (extra-switches (list "--resume" session-id))
               (buffer (claude-code--start nil extra-switches nil nil)))
          (when buffer
            ;; Register session using correlation-id for callback lookup
            (task-conductor-dev-env--register-session correlation-id buffer)
            ;; Schedule completion after delay
            (run-at-time integration-test-session-delay nil
                         #'integration-test--complete-session
                         correlation-id "completed"
                         '((state . "idle") (reason . "test"))
                         0)))
      (error
       (message "integration-test: Error in open-session: %s" (error-message-string err))
       (message "ERROR:%s" (error-message-string err))
       (task-conductor-dev-env-send-error correlation-id (error-message-string err))))))

;;; Main entry point

(defun integration-test-run ()
  "Run the integration test server."
  (message "integration-test: Starting...")

  ;; Install mocks before starting server
  (integration-test--install-mocks)

  ;; Override the open-session handler
  (setf (alist-get "open-session" task-conductor-dev-env-message-handlers
                   nil nil #'string=)
        #'integration-test--handle-open-session)

  ;; Use test socket path
  (setq task-conductor-dev-env-socket-path integration-test-socket-path)
  (setq task-conductor-dev-env-debug t)

  ;; Clean up any stale socket
  (when (file-exists-p integration-test-socket-path)
    (delete-file integration-test-socket-path))

  ;; Start the server
  (task-conductor-dev-env-start)

  (if (task-conductor-dev-env-running-p)
      (progn
        (message "integration-test: Server started on %s" integration-test-socket-path)
        ;; Use message instead of princ for reliable output in batch mode
        (message "READY")
        ;; Event loop - keep running until timeout
        (let ((start-time (float-time)))
          (while (and (task-conductor-dev-env-running-p)
                      (< (- (float-time) start-time) integration-test-timeout))
            (sit-for 0.1))))
    (progn
      (message "integration-test: Failed to start server")
      (message "ERROR:Failed to start server")
      (kill-emacs 1)))

  ;; Cleanup
  (message "integration-test: Shutting down...")
  (task-conductor-dev-env-stop)
  (message "integration-test: Done"))

(provide 'integration-test-helper)
;;; integration-test-helper.el ends here
