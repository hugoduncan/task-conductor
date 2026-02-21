;;; claude-code.el --- Stub for claude-code.el -*- lexical-binding: t; -*-

;; This is a test stub providing the minimal interface needed by
;; task-conductor-dev-env.el for compilation and testing.

;;; Code:

(defvar claude-code--stub-buffer nil
  "Buffer returned by stub `claude-code--start'.")

(defun claude-code--start (_dir args)
  "Stub for claude-code--start.
Returns a buffer or the value of `claude-code--stub-buffer'.
ARGS should include --resume and session-id."
  (or claude-code--stub-buffer
      (generate-new-buffer "*claude-code-stub*")))

(defun claude-code--find-claude-buffers-for-directory (_dir)
  "Stub for claude-code--find-claude-buffers-for-directory.
Returns a list containing `claude-code--stub-buffer' if set."
  (when claude-code--stub-buffer
    (list claude-code--stub-buffer)))

(defun claude-code--find-all-claude-buffers ()
  "Stub for claude-code--find-all-claude-buffers.
Returns a list containing `claude-code--stub-buffer' if set."
  (when claude-code--stub-buffer
    (list claude-code--stub-buffer)))

(provide 'claude-code)
;;; claude-code.el ends here
