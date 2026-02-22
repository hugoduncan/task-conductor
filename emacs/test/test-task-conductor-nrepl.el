;;; test-task-conductor-nrepl.el --- Tests for task-conductor-nrepl -*- lexical-binding: t; -*-

;; Tests for the minimal nREPL client used by task-conductor.
;; These tests verify bencode encoding/decoding, dict-get, and
;; multi-message accumulation without requiring a running nREPL server.

(require 'ert)
(require 'task-conductor-nrepl)

;;; Bencode encode tests

(ert-deftest task-conductor-nrepl-encode-string ()
  ;; Verifies string encoding with byte-length prefix,
  ;; including multi-byte UTF-8 characters.
  (should (equal "5:hello"
                 (task-conductor-nrepl--bencode-encode "hello")))
  (should (equal "0:"
                 (task-conductor-nrepl--bencode-encode "")))
  ;; Multi-byte: "日本" is 6 UTF-8 bytes, 2 characters
  (should (equal (format "6:%s" "日本")
                 (task-conductor-nrepl--bencode-encode "日本")))
  ;; Mixed ASCII + multi-byte: "hi" = 2 bytes, "日本" = 6 bytes = 8
  (should (equal (format "8:%s" "hi日本")
                 (task-conductor-nrepl--bencode-encode "hi日本"))))

(ert-deftest task-conductor-nrepl-encode-integer ()
  ;; Verifies integer encoding in i<n>e format.
  (should (equal "i42e"
                 (task-conductor-nrepl--bencode-encode 42)))
  (should (equal "i0e"
                 (task-conductor-nrepl--bencode-encode 0)))
  (should (equal "i-1e"
                 (task-conductor-nrepl--bencode-encode -1))))

(ert-deftest task-conductor-nrepl-encode-list ()
  ;; Verifies list encoding as bencode lists.
  (should (equal "l3:foo3:bare"
                 (task-conductor-nrepl--bencode-encode '("foo" "bar"))))
  (should (equal "le"
                 (task-conductor-nrepl--bencode-encode '()))))

(ert-deftest task-conductor-nrepl-encode-dict ()
  ;; Verifies alist-with-string-keys encoding as bencode dicts.
  (should (equal "d2:op4:eval4:code5:helloe"
                 (task-conductor-nrepl--bencode-encode
                  '(("op" . "eval") ("code" . "hello"))))))

;;; Bencode decode tests

(ert-deftest task-conductor-nrepl-decode-string ()
  ;; Verifies string decoding and incomplete-input handling,
  ;; including multi-byte UTF-8 on a binary (unibyte) buffer.
  (let ((result (task-conductor-nrepl--bencode-decode "5:hellorest")))
    (should (equal "hello" (car result)))
    (should (equal "rest" (cdr result))))
  (should-not (task-conductor-nrepl--bencode-decode "5:hel"))
  ;; Multi-byte: "日本" = 6 UTF-8 bytes. Simulate binary buffer input.
  (let* ((raw (encode-coding-string (format "6:%s" "日本") 'utf-8))
         (result (task-conductor-nrepl--bencode-decode raw)))
    (should result)
    (should (equal "日本" (car result)))
    (should (equal "" (cdr result))))
  ;; Mixed ASCII + multi-byte with trailing data
  (let* ((raw (encode-coding-string (format "8:%sxyz" "hi日本") 'utf-8))
         (result (task-conductor-nrepl--bencode-decode raw)))
    (should result)
    (should (equal "hi日本" (car result)))
    (should (equal "xyz" (cdr result)))))

(ert-deftest task-conductor-nrepl-decode-integer ()
  ;; Verifies integer decoding and incomplete-input handling.
  (let ((result (task-conductor-nrepl--bencode-decode "i42erest")))
    (should (equal 42 (car result)))
    (should (equal "rest" (cdr result))))
  (should-not (task-conductor-nrepl--bencode-decode "i42")))

(ert-deftest task-conductor-nrepl-decode-list ()
  ;; Verifies list decoding and incomplete-input handling.
  (let ((result (task-conductor-nrepl--bencode-decode "l3:fooi5ee")))
    (should (equal '("foo" 5) (car result)))
    (should (equal "" (cdr result))))
  (should-not (task-conductor-nrepl--bencode-decode "l3:foo")))

(ert-deftest task-conductor-nrepl-decode-dict ()
  ;; Verifies dict decoding as alist and incomplete-input handling.
  (let ((result (task-conductor-nrepl--bencode-decode "d2:op4:evale")))
    (should (equal '(("op" . "eval")) (car result)))
    (should (equal "" (cdr result))))
  (should-not (task-conductor-nrepl--bencode-decode "d2:op")))

;;; dict-get tests

(ert-deftest task-conductor-nrepl-dict-get-test ()
  ;; Verifies key lookup in response alists.
  (should (equal "bar"
                 (task-conductor-nrepl-dict-get '(("foo" . "bar")) "foo")))
  (should-not (task-conductor-nrepl-dict-get '(("foo" . "bar")) "baz")))

;;; Multi-message accumulation tests

(ert-deftest task-conductor-nrepl-accumulate-concat-out ()
  ;; Verifies that `out' values are concatenated across chunks
  ;; and the callback fires on `done' with the merged response.
  (let ((conn (list :process nil :session "s1" :buffer ""
                    :pending (make-hash-table :test 'equal)
                    :counter 0
                    :accumulator (make-hash-table :test 'equal))))
    ;; First chunk: partial output, should not be done yet
    (should-not
     (task-conductor-nrepl--accumulate
      conn "1" '(("out" . "hello "))))
    ;; Second chunk: more output + value + done
    (let ((final (task-conductor-nrepl--accumulate
                  conn "1" '(("out" . "world")
                             ("value" . "nil")
                             ("status" "done")))))
      (should final)
      (should (equal "hello world"
                     (task-conductor-nrepl-dict-get final "out")))
      (should (equal "nil"
                     (task-conductor-nrepl-dict-get final "value"))))))

(ert-deftest task-conductor-nrepl-accumulate-err ()
  ;; Verifies that `err' values are concatenated across chunks.
  (let ((conn (list :process nil :session "s1" :buffer ""
                    :pending (make-hash-table :test 'equal)
                    :counter 0
                    :accumulator (make-hash-table :test 'equal))))
    (task-conductor-nrepl--accumulate conn "2" '(("err" . "warn: ")))
    (let ((final (task-conductor-nrepl--accumulate
                  conn "2" '(("err" . "something")
                             ("status" "done")))))
      (should (equal "warn: something"
                     (task-conductor-nrepl-dict-get final "err"))))))

(ert-deftest task-conductor-nrepl-filter-drives-callback ()
  ;; Drives the filter with chunked bencode data and verifies the
  ;; callback fires once with the merged response.
  (let* ((conn (list :process nil :session "s1" :buffer ""
                     :pending (make-hash-table :test 'equal)
                     :counter 0
                     :accumulator (make-hash-table :test 'equal)))
         (callback-result nil))
    ;; Register a callback for id "42"
    (puthash "42"
             (lambda (resp) (setq callback-result resp))
             (plist-get conn :pending))
    ;; Feed first message: partial output
    (let ((msg1 (task-conductor-nrepl--bencode-encode
                 '(("id" . "42") ("out" . "hi ")))))
      (task-conductor-nrepl--filter conn msg1))
    (should-not callback-result)
    ;; Feed second message: value + done
    (let ((msg2 (task-conductor-nrepl--bencode-encode
                 '(("id" . "42") ("value" . "ok")
                   ("status" "done")))))
      (task-conductor-nrepl--filter conn msg2))
    (should callback-result)
    (should (equal "hi "
                   (task-conductor-nrepl-dict-get callback-result "out")))
    (should (equal "ok"
                   (task-conductor-nrepl-dict-get callback-result "value")))))

(provide 'test-task-conductor-nrepl)
;;; test-task-conductor-nrepl.el ends here
