;;; parseedn.el --- Stub for parseedn -*- lexical-binding: t; -*-

;; Minimal stub for testing.  Only provides the symbols needed
;; by task-conductor-dev-env.el at compile/load time.

;;; Code:

(defun parseedn-read-str (str)
  "Stub for parseedn-read-str.
STR is an EDN string.  Returns nil in test context."
  (ignore str)
  nil)

(provide 'parseedn)
;;; parseedn.el ends here
