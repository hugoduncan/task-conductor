;;; task-conductor-dev-env-test.el --- Tests for task-conductor-dev-env -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for task-conductor-dev-env message parsing and dispatch.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'parseedn)
(require 'task-conductor-dev-env)

;;; Message Parsing Tests

(ert-deftest task-conductor-dev-env-test-parse-open-session ()
  "Parse open-session message with all fields."
  (let ((result (task-conductor-dev-env--parse-message
                 "{\"type\":\"open-session\",\"session-id\":\"abc-123\",\"prompt\":\"hello\",\"working-dir\":\"/tmp\"}")))
    (should (equal (alist-get 'type result) "open-session"))
    (should (equal (alist-get 'session-id result) "abc-123"))
    (should (equal (alist-get 'prompt result) "hello"))
    (should (equal (alist-get 'working-dir result) "/tmp"))))

(ert-deftest task-conductor-dev-env-test-parse-close-session ()
  "Parse close-session message."
  (let ((result (task-conductor-dev-env--parse-message
                 "{\"type\":\"close-session\",\"session-id\":\"abc-123\"}")))
    (should (equal (alist-get 'type result) "close-session"))
    (should (equal (alist-get 'session-id result) "abc-123"))))

(ert-deftest task-conductor-dev-env-test-parse-optional-fields ()
  "Parse open-session message with optional fields missing."
  (let ((result (task-conductor-dev-env--parse-message
                 "{\"type\":\"open-session\",\"session-id\":\"abc-123\"}")))
    (should (equal (alist-get 'type result) "open-session"))
    (should (equal (alist-get 'session-id result) "abc-123"))
    (should (null (alist-get 'prompt result)))
    (should (null (alist-get 'working-dir result)))))

(ert-deftest task-conductor-dev-env-test-parse-invalid-json ()
  "Return nil for invalid JSON."
  (should (null (task-conductor-dev-env--parse-message "not json")))
  (should (null (task-conductor-dev-env--parse-message "{invalid")))
  (should (null (task-conductor-dev-env--parse-message ""))))

(ert-deftest task-conductor-dev-env-test-parse-empty-object ()
  "Parse empty JSON object.
Empty JSON object {} becomes nil in Emacs Lisp (empty alist)."
  (let ((result (task-conductor-dev-env--parse-message "{}")))
    ;; Empty object is nil in elisp, which is falsy but not an error
    (should (null result))
    (should (null (alist-get 'type result)))))

;;; Message Dispatch Tests

(ert-deftest task-conductor-dev-env-test-dispatch-open-session ()
  "Dispatch routes open-session to handler."
  (let ((called nil))
    (cl-letf (((symbol-function 'task-conductor-dev-env--handle-open-session)
               (lambda (msg) (setq called msg))))
      (task-conductor-dev-env--dispatch-message '((type . "open-session") (session-id . "test")))
      (should called)
      (should (equal (alist-get 'session-id called) "test")))))

(ert-deftest task-conductor-dev-env-test-dispatch-close-session ()
  "Dispatch routes close-session to handler."
  (let ((called nil))
    (cl-letf (((symbol-function 'task-conductor-dev-env--handle-close-session)
               (lambda (msg) (setq called msg))))
      (task-conductor-dev-env--dispatch-message '((type . "close-session") (session-id . "test")))
      (should called)
      (should (equal (alist-get 'session-id called) "test")))))

(ert-deftest task-conductor-dev-env-test-dispatch-unknown-type ()
  "Dispatch ignores unknown message types."
  (let ((open-called nil)
        (close-called nil))
    (cl-letf (((symbol-function 'task-conductor-dev-env--handle-open-session)
               (lambda (_) (setq open-called t)))
              ((symbol-function 'task-conductor-dev-env--handle-close-session)
               (lambda (_) (setq close-called t))))
      (task-conductor-dev-env--dispatch-message '((type . "unknown") (session-id . "test")))
      (should-not open-called)
      (should-not close-called))))

;;; JSON Encoding Tests

(ert-deftest task-conductor-dev-env-test-encode-session-complete ()
  "Verify session-complete message structure."
  (let ((json-encoding-pretty-print nil))
    (let ((encoded (json-encode
                    '((type . "session-complete")
                      (session-id . "abc-123")
                      (status . "completed")
                      (hook-status . ((idle . t)))
                      (exit-code . 0)))))
      (should (string-match-p "\"type\":\"session-complete\"" encoded))
      (should (string-match-p "\"session-id\":\"abc-123\"" encoded))
      (should (string-match-p "\"status\":\"completed\"" encoded))
      (should (string-match-p "\"exit-code\":0" encoded)))))

(ert-deftest task-conductor-dev-env-test-encode-error ()
  "Verify error message structure."
  (let ((json-encoding-pretty-print nil))
    (let ((encoded (json-encode
                    '((type . "error")
                      (session-id . "abc-123")
                      (message . "Connection failed")))))
      (should (string-match-p "\"type\":\"error\"" encoded))
      (should (string-match-p "\"session-id\":\"abc-123\"" encoded))
      (should (string-match-p "\"message\":\"Connection failed\"" encoded)))))

;;; Server State Tests

(ert-deftest task-conductor-dev-env-test-initial-state ()
  "Server and client processes are nil initially."
  (should (null task-conductor-dev-env--server-process))
  (should (null task-conductor-dev-env--client-process)))

(ert-deftest task-conductor-dev-env-test-running-p-when-stopped ()
  "running-p returns nil when server not started."
  (let ((task-conductor-dev-env--server-process nil))
    (should-not (task-conductor-dev-env-running-p))))

(ert-deftest task-conductor-dev-env-test-connected-p-when-no-client ()
  "connected-p returns nil when no client connected."
  (let ((task-conductor-dev-env--client-process nil))
    (should-not (task-conductor-dev-env-connected-p))))

;;; Session State Management Tests

(ert-deftest task-conductor-dev-env-test-register-session ()
  "Register session stores in both hash tables."
  (let ((task-conductor-dev-env--sessions (make-hash-table :test 'equal))
        (task-conductor-dev-env--buffer-sessions (make-hash-table :test 'eq)))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (task-conductor-dev-env--register-session "test-123" buf)
        (should (eq (gethash "test-123" task-conductor-dev-env--sessions) buf))
        (should (equal (gethash buf task-conductor-dev-env--buffer-sessions) "test-123"))))))

(ert-deftest task-conductor-dev-env-test-unregister-session ()
  "Unregister session removes from both hash tables."
  (let ((task-conductor-dev-env--sessions (make-hash-table :test 'equal))
        (task-conductor-dev-env--buffer-sessions (make-hash-table :test 'eq)))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (task-conductor-dev-env--register-session "test-123" buf)
        (task-conductor-dev-env--unregister-session "test-123")
        (should (null (gethash "test-123" task-conductor-dev-env--sessions)))
        (should (null (gethash buf task-conductor-dev-env--buffer-sessions)))))))

(ert-deftest task-conductor-dev-env-test-get-buffer ()
  "Get buffer returns registered buffer for session-id."
  (let ((task-conductor-dev-env--sessions (make-hash-table :test 'equal))
        (task-conductor-dev-env--buffer-sessions (make-hash-table :test 'eq)))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (task-conductor-dev-env--register-session "test-123" buf)
        (should (eq (task-conductor-dev-env--get-buffer "test-123") buf))
        (should (null (task-conductor-dev-env--get-buffer "nonexistent")))))))

(ert-deftest task-conductor-dev-env-test-get-session-id ()
  "Get session-id returns registered session-id for buffer."
  (let ((task-conductor-dev-env--sessions (make-hash-table :test 'equal))
        (task-conductor-dev-env--buffer-sessions (make-hash-table :test 'eq)))
    (with-temp-buffer
      (let ((buf (current-buffer)))
        (task-conductor-dev-env--register-session "test-123" buf)
        (should (equal (task-conductor-dev-env--get-session-id buf) "test-123"))))
    (with-temp-buffer
      (should (null (task-conductor-dev-env--get-session-id (current-buffer)))))))

(ert-deftest task-conductor-dev-env-test-multiple-sessions ()
  "Multiple sessions can be registered and tracked."
  (let ((task-conductor-dev-env--sessions (make-hash-table :test 'equal))
        (task-conductor-dev-env--buffer-sessions (make-hash-table :test 'eq)))
    (let ((buf1 (generate-new-buffer " *test-1*"))
          (buf2 (generate-new-buffer " *test-2*")))
      (unwind-protect
          (progn
            (task-conductor-dev-env--register-session "session-1" buf1)
            (task-conductor-dev-env--register-session "session-2" buf2)
            (should (eq (task-conductor-dev-env--get-buffer "session-1") buf1))
            (should (eq (task-conductor-dev-env--get-buffer "session-2") buf2))
            (should (equal (task-conductor-dev-env--get-session-id buf1) "session-1"))
            (should (equal (task-conductor-dev-env--get-session-id buf2) "session-2")))
        (kill-buffer buf1)
        (kill-buffer buf2)))))

;;; EDN Parsing Tests

(ert-deftest task-conductor-dev-env-test-edn-to-alist-simple-map ()
  "Convert simple EDN map to alist."
  (let ((hash (make-hash-table)))
    (puthash :status :completed hash)
    (puthash :timestamp "2025-01-16T12:00:00Z" hash)
    (let ((result (task-conductor-dev-env--edn-to-alist hash)))
      (should (equal (alist-get 'status result) "completed"))
      (should (equal (alist-get 'timestamp result) "2025-01-16T12:00:00Z")))))

(ert-deftest task-conductor-dev-env-test-edn-to-alist-nested-map ()
  "Convert nested EDN map to alist."
  (let ((inner (make-hash-table))
        (outer (make-hash-table)))
    (puthash :reason :cli-killed inner)
    (puthash :status :error outer)
    (puthash :details inner outer)
    (let ((result (task-conductor-dev-env--edn-to-alist outer)))
      (should (equal (alist-get 'status result) "error"))
      (let ((details (alist-get 'details result)))
        (should (equal (alist-get 'reason details) "cli-killed"))))))

(ert-deftest task-conductor-dev-env-test-edn-to-alist-preserves-strings ()
  "String values are preserved as-is."
  (let ((hash (make-hash-table)))
    (puthash :message "Use :foo here" hash)
    (let ((result (task-conductor-dev-env--edn-to-alist hash)))
      (should (equal (alist-get 'message result) "Use :foo here")))))

(ert-deftest task-conductor-dev-env-test-edn-to-alist-handles-numbers ()
  "Numeric values are preserved."
  (let ((hash (make-hash-table)))
    (puthash :exit-code 42 hash)
    (puthash :count 0 hash)
    (let ((result (task-conductor-dev-env--edn-to-alist hash)))
      (should (equal (alist-get 'exit-code result) 42))
      (should (equal (alist-get 'count result) 0)))))

(ert-deftest task-conductor-dev-env-test-read-handoff-edn-nonexistent ()
  "Return nil for nonexistent file."
  (let ((result (task-conductor-dev-env--read-handoff-edn "/nonexistent/path")))
    (should (null result))))

(ert-deftest task-conductor-dev-env-test-read-handoff-edn-valid ()
  "Parse valid handoff.edn file."
  (let ((temp-dir (make-temp-file "tc-test" t)))
    (unwind-protect
        (let ((tc-dir (expand-file-name ".task-conductor" temp-dir)))
          (make-directory tc-dir)
          (with-temp-file (expand-file-name "handoff.edn" tc-dir)
            (insert "{:status :completed :timestamp \"2025-01-16T12:00:00Z\"}"))
          (let ((result (task-conductor-dev-env--read-handoff-edn temp-dir)))
            (should result)
            (should (equal (alist-get 'status result) "completed"))
            (should (equal (alist-get 'timestamp result) "2025-01-16T12:00:00Z"))))
      (delete-directory temp-dir t))))

(ert-deftest task-conductor-dev-env-test-read-handoff-edn-with-reason ()
  "Parse handoff.edn with optional reason field."
  (let ((temp-dir (make-temp-file "tc-test" t)))
    (unwind-protect
        (let ((tc-dir (expand-file-name ".task-conductor" temp-dir)))
          (make-directory tc-dir)
          (with-temp-file (expand-file-name "handoff.edn" tc-dir)
            (insert "{:status :error :timestamp \"2025-01-16T12:00:00Z\" :reason :cli-killed}"))
          (let ((result (task-conductor-dev-env--read-handoff-edn temp-dir)))
            (should result)
            (should (equal (alist-get 'status result) "error"))
            (should (equal (alist-get 'reason result) "cli-killed"))))
      (delete-directory temp-dir t))))

;;; Event Hook Handler Tests

(ert-deftest task-conductor-dev-env-test-event-hook-ignores-untracked ()
  "Event hook ignores events for untracked sessions."
  (let ((task-conductor-dev-env--sessions (make-hash-table :test 'equal))
        (task-conductor-dev-env--buffer-sessions (make-hash-table :test 'eq))
        (sent-messages nil))
    (cl-letf (((symbol-function 'task-conductor-dev-env--send-message)
               (lambda (msg) (push msg sent-messages))))
      (task-conductor-dev-env--event-hook-handler
       '(:type "stop" :buffer-name "*claude:test*"))
      (should (null sent-messages)))))

(ert-deftest task-conductor-dev-env-test-event-hook-handles-stop ()
  "Event hook sends session-complete on stop event."
  (let ((task-conductor-dev-env--sessions (make-hash-table :test 'equal))
        (task-conductor-dev-env--buffer-sessions (make-hash-table :test 'eq))
        (sent-messages nil))
    (let ((buf (generate-new-buffer "*claude:test*")))
      (unwind-protect
          (progn
            (task-conductor-dev-env--register-session "test-123" buf)
            (cl-letf (((symbol-function 'task-conductor-dev-env--send-message)
                       (lambda (msg) (push msg sent-messages))))
              (task-conductor-dev-env--event-hook-handler
               `(:type "stop" :buffer-name ,(buffer-name buf)))
              (should (= (length sent-messages) 1))
              (let ((msg (car sent-messages)))
                (should (equal (alist-get 'type msg) "session-complete"))
                (should (equal (alist-get 'session-id msg) "test-123"))
                (should (equal (alist-get 'status msg) "completed")))
              ;; Session should be unregistered after completion
              (should (null (task-conductor-dev-env--get-buffer "test-123")))))
        (when (buffer-live-p buf)
          (kill-buffer buf))))))

(provide 'task-conductor-dev-env-test)
;;; task-conductor-dev-env-test.el ends here
