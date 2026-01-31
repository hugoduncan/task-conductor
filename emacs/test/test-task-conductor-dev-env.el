;;; test-task-conductor-dev-env.el --- Tests for task-conductor-dev-env -*- lexical-binding: t; -*-

;; Tests for the Emacs dev-env component of task-conductor.
;; These tests mock external dependencies (cider, claude-code) to verify
;; the package's internal logic without requiring a running nREPL server.

;;; Manual Integration Testing
;;
;; This section describes how to manually test the integration between
;; task-conductor-dev-env.el and the Clojure emacs-dev-env component
;; with actual Emacs and claude-code.el.
;;
;; Prerequisites:
;; - Emacs 28.1+ with cider and claude-code.el installed
;; - Claude CLI installed and authenticated
;; - JVM with task-conductor running
;;
;; Step 1: Start the task-conductor REPL with nREPL
;;
;;   cd /path/to/task-conductor
;;   clj -M:dev:nrepl
;;
;; Step 2: In Emacs, connect to the nREPL server
;;
;;   M-x cider-connect RET localhost RET <port> RET
;;
;;   The port is typically written to .nrepl-port in the project directory.
;;
;; Step 3: Load the task-conductor-dev-env package
;;
;;   M-x load-file RET /path/to/emacs/task-conductor-dev-env.el RET
;;
;; Step 4: Connect to task-conductor as a dev-env
;;
;;   M-x task-conductor-dev-env-connect RET
;;
;;   You should see: "Connected to task-conductor as dev-env: <uuid>"
;;   This registers Emacs with the orchestrator and starts the poll loop.
;;
;; Step 5: Verify registration from the REPL
;;
;;   In the Clojure REPL, run:
;;     (require '[task-conductor.emacs-dev-env.interface :as ede])
;;     (ede/list-dev-envs)
;;     ;; => [{:dev-env-id "abc-123" :type :emacs :connected? true}]
;;
;;     (ede/list-healthy-dev-envs)
;;     ;; => [{:dev-env-id "abc-123" :type :emacs :connected? true}]
;;     ;; Emacs should respond to the ping command
;;
;; Step 6: Test start-session command
;;
;;   From the Clojure REPL:
;;     (def dev-env (ede/select-dev-env))
;;     (require '[task-conductor.dev-env.protocol :as proto])
;;     (proto/start-session (:dev-env dev-env) "test-session-1" {})
;;     ;; => {:status :ok :buffer-name "*claude: ..."}
;;
;;   A new claude-code buffer should appear in Emacs with Claude CLI running.
;;
;; Step 7: Test hook registration
;;
;;   From the Clojure REPL:
;;     (def on-idle-callback (fn [ctx] (println "Idle event:" ctx)))
;;     (proto/register-hook (:dev-env dev-env) :on-idle on-idle-callback)
;;
;;   When Claude goes idle (bell character detected), you should see
;;   "Idle event: {...}" printed in the REPL after a 0.5s debounce.
;;
;; Step 8: Test query-transcript
;;
;;   From the Clojure REPL:
;;     (proto/query-transcript (:dev-env dev-env) "test-session-1")
;;     ;; => {:status :ok :transcript "...buffer content..."}
;;
;; Step 9: Test close-session
;;
;;   From the Clojure REPL:
;;     (proto/close-session (:dev-env dev-env) "test-session-1")
;;     ;; => {:status :ok}
;;
;;   The claude-code buffer should be killed in Emacs.
;;
;; Step 10: Disconnect
;;
;;   In Emacs:
;;     M-x task-conductor-dev-env-disconnect RET
;;
;;   You should see: "Disconnected from task-conductor"
;;
;; Troubleshooting:
;;
;; - "Not connected to CIDER": Run M-x cider-connect first
;; - "nREPL eval error": Check that emacs-dev-env component is on classpath
;; - "command channel closed": The JVM may have restarted; reconnect
;; - Poll loop errors appear in *Messages* buffer
;; - Use (ede/ping-by-id "<id>") to test connectivity
;; - Check @task-conductor.emacs-dev-env.core/registry for registered envs

(require 'ert)
(require 'cl-lib)
(require 'task-conductor-dev-env)

;;; Test Helpers

(defmacro with-task-conductor-test-state (&rest body)
  "Execute BODY with clean task-conductor state."
  (declare (indent 0))
  `(let ((task-conductor-dev-env--dev-env-id nil)
         (task-conductor-dev-env--poll-timer nil)
         (task-conductor-dev-env--sessions (make-hash-table :test 'equal))
         (task-conductor-dev-env--session-hooks (make-hash-table :test 'equal)))
     ,@body))

(defmacro with-mock-cider (&rest body)
  "Execute BODY with mocked CIDER functions."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'cider-connected-p) (lambda () t))
             ((symbol-function 'cider-nrepl-sync-request:eval)
              (lambda (form)
                (list "value" (prin1-to-string '(:status :ok)))))
             ((symbol-function 'nrepl-dict-get)
              (lambda (dict key)
                (plist-get dict (intern key)))))
     ,@body))

;;; UUID Generation Tests

(ert-deftest task-conductor-dev-env-uuid-format ()
  ;; Test that generated UUIDs match the v4 format.
  (let ((uuid (task-conductor-dev-env--generate-uuid)))
    (should (stringp uuid))
    (should (= (length uuid) 36))
    (should (string-match-p
             "^[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-4[0-9a-f]\\{3\\}-[89ab][0-9a-f]\\{3\\}-[0-9a-f]\\{12\\}$"
             uuid))))

(ert-deftest task-conductor-dev-env-uuid-uniqueness ()
  ;; Test that consecutive UUIDs are different.
  (let ((uuid1 (task-conductor-dev-env--generate-uuid))
        (uuid2 (task-conductor-dev-env--generate-uuid)))
    (should-not (string= uuid1 uuid2))))

;;; ANSI Stripping Tests

(ert-deftest task-conductor-dev-env-strip-ansi-codes ()
  ;; Test ANSI escape code removal.
  (should (string= "hello world"
                   (task-conductor-dev-env--strip-ansi-codes
                    "\033[31mhello\033[0m \033[1;32mworld\033[0m")))
  (should (string= "plain text"
                   (task-conductor-dev-env--strip-ansi-codes "plain text")))
  (should (string= ""
                   (task-conductor-dev-env--strip-ansi-codes ""))))

;;; Session Management Tests

(ert-deftest task-conductor-dev-env-find-session-for-buffer ()
  ;; Test session lookup by buffer.
  (with-task-conductor-test-state
    (let ((buf (generate-new-buffer "*test-session*")))
      (unwind-protect
          (progn
            (puthash "session-123" buf task-conductor-dev-env--sessions)
            (should (string= "session-123"
                             (task-conductor-dev-env--find-session-for-buffer buf)))
            (should-not (task-conductor-dev-env--find-session-for-buffer
                         (generate-new-buffer "*other*"))))
        (kill-buffer buf)))))

;;; Command Handler Tests

(ert-deftest task-conductor-dev-env-handle-start-session-missing-id ()
  ;; Test that start-session requires session-id.
  (with-task-conductor-test-state
    (let ((result (task-conductor-dev-env--handle-start-session '())))
      (should (eq :error (plist-get result :status)))
      (should (string-match-p "Missing" (plist-get result :message))))))

(ert-deftest task-conductor-dev-env-handle-start-session-creates-buffer ()
  ;; Test that start-session creates a claude-code buffer.
  (with-task-conductor-test-state
    (let* ((test-buffer (generate-new-buffer "*test-claude*"))
           (claude-code--stub-buffer test-buffer))
      (unwind-protect
          (let ((result (task-conductor-dev-env--handle-start-session
                         '(:session-id "sess-abc"))))
            (should (eq :ok (plist-get result :status)))
            (should (gethash "sess-abc" task-conductor-dev-env--sessions)))
        (kill-buffer test-buffer)))))

(ert-deftest task-conductor-dev-env-handle-start-session-reuses-existing ()
  ;; Test that start-session reuses existing buffer for same session.
  (with-task-conductor-test-state
    (let ((existing-buffer (generate-new-buffer "*existing*")))
      (unwind-protect
          (progn
            (puthash "sess-existing" existing-buffer task-conductor-dev-env--sessions)
            (let ((result (task-conductor-dev-env--handle-start-session
                           '(:session-id "sess-existing"))))
              (should (eq :ok (plist-get result :status)))
              (should (string= (buffer-name existing-buffer)
                               (plist-get result :buffer-name)))))
        (kill-buffer existing-buffer)))))

(ert-deftest task-conductor-dev-env-handle-register-hook-missing-params ()
  ;; Test that register-hook validates required parameters.
  (with-task-conductor-test-state
    (let ((result1 (task-conductor-dev-env--handle-register-hook '()))
          (result2 (task-conductor-dev-env--handle-register-hook
                    '(:session-id "sess"))))
      (should (eq :error (plist-get result1 :status)))
      (should (eq :error (plist-get result2 :status))))))

(ert-deftest task-conductor-dev-env-handle-register-hook-invalid-type ()
  ;; Test that register-hook rejects invalid hook types.
  (with-task-conductor-test-state
    (let ((result (task-conductor-dev-env--handle-register-hook
                   '(:session-id "sess" :hook-type :invalid))))
      (should (eq :error (plist-get result :status)))
      (should (string-match-p "Invalid" (plist-get result :message))))))

(ert-deftest task-conductor-dev-env-handle-query-transcript-missing-session ()
  ;; Test that query-transcript requires session-id.
  (with-task-conductor-test-state
    (let ((result (task-conductor-dev-env--handle-query-transcript '())))
      (should (eq :error (plist-get result :status))))))

(ert-deftest task-conductor-dev-env-handle-query-transcript-returns-content ()
  ;; Test that query-transcript returns buffer content.
  (with-task-conductor-test-state
    (let ((buf (generate-new-buffer "*transcript-test*")))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (insert "Hello from Claude"))
            (puthash "sess-t" buf task-conductor-dev-env--sessions)
            (let ((result (task-conductor-dev-env--handle-query-transcript
                           '(:session-id "sess-t"))))
              (should (eq :ok (plist-get result :status)))
              (should (string= "Hello from Claude"
                               (plist-get result :transcript)))))
        (kill-buffer buf)))))

(ert-deftest task-conductor-dev-env-handle-close-session-missing-id ()
  ;; Test that close-session requires session-id.
  (with-task-conductor-test-state
    (let ((result (task-conductor-dev-env--handle-close-session '())))
      (should (eq :error (plist-get result :status))))))

(ert-deftest task-conductor-dev-env-handle-close-session-kills-buffer ()
  ;; Test that close-session kills the session buffer.
  (with-task-conductor-test-state
    (let ((buf (generate-new-buffer "*close-test*")))
      (puthash "sess-close" buf task-conductor-dev-env--sessions)
      (let ((result (task-conductor-dev-env--handle-close-session
                     '(:session-id "sess-close"))))
        (should (eq :ok (plist-get result :status)))
        (should-not (buffer-live-p buf))
        (should-not (gethash "sess-close" task-conductor-dev-env--sessions))))))

;;; Command Dispatch Tests

(ert-deftest task-conductor-dev-env-dispatch-ping ()
  ;; Test that ping command returns ok status.
  (with-task-conductor-test-state
    (with-mock-cider
      (setq task-conductor-dev-env--dev-env-id "test-id")
      (let ((response (task-conductor-dev-env--dispatch-command
                       '(:command-id "cmd-1" :command :ping :params nil))))
        (should (eq :ok (plist-get response :status)))))))

(ert-deftest task-conductor-dev-env-dispatch-unknown-command ()
  ;; Test that unknown commands return error.
  (with-task-conductor-test-state
    (with-mock-cider
      (setq task-conductor-dev-env--dev-env-id "test-id")
      (let ((response (task-conductor-dev-env--dispatch-command
                       '(:command-id "cmd-2" :command :unknown :params nil))))
        (should (eq :unknown-command (plist-get response :error)))))))

;;; State Cleanup Tests

(ert-deftest task-conductor-dev-env-cleanup-session-hooks ()
  ;; Test that cleanup removes hooks and cancels timers.
  (with-task-conductor-test-state
    (let ((timer (run-with-timer 1000 nil #'ignore)))
      (puthash "sess-cleanup"
               (list :on-idle (list :hook-id "h1" :timer timer))
               task-conductor-dev-env--session-hooks)
      (task-conductor-dev-env--cleanup-session-hooks "sess-cleanup")
      (should-not (gethash "sess-cleanup" task-conductor-dev-env--session-hooks)))))

(provide 'test-task-conductor-dev-env)
;;; test-task-conductor-dev-env.el ends here
