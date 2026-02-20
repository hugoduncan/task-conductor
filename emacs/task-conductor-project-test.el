;;; task-conductor-project-test.el --- Tests for task-conductor-project -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the project buffer rendering logic.

;;; Code:

(require 'ert)
(require 'task-conductor-project)

(defmacro with-project-buffer (&rest body)
  "Execute BODY in a temporary project buffer with project mode active."
  (declare (indent 0))
  `(with-temp-buffer
     (task-conductor-project-mode)
     ,@body))

(ert-deftest task-conductor-project-render-empty ()
  ;; Rendering with no projects shows the header and "(none)".
  (with-project-buffer
    (task-conductor-project--render nil)
    (let ((text (buffer-string)))
      (should (string-match-p "Projects (0)" text))
      (should (string-match-p "(none)" text)))))

(ert-deftest task-conductor-project-render-single ()
  ;; Rendering a single project shows name and path.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "myproj" :project/path "/tmp/myproj")))
    (let ((text (buffer-string)))
      (should (string-match-p "Projects (1)" text))
      (should (string-match-p "myproj" text))
      (should (string-match-p "/tmp/myproj" text)))))

(ert-deftest task-conductor-project-render-multiple ()
  ;; Rendering multiple projects aligns names by padding.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "ab" :project/path "/a")
           (list :project/name "longname" :project/path "/b")))
    (let ((text (buffer-string)))
      (should (string-match-p "Projects (2)" text))
      (should (string-match-p "ab" text))
      (should (string-match-p "longname" text)))))

(ert-deftest task-conductor-project-format-entry-padding ()
  ;; Format pads name to the given width.
  (let ((entry (task-conductor-project--format-entry
                (list :project/name "ab" :project/path "/x")
                10)))
    (should (string-match-p "^ab " entry))
    (should (string-match-p "/x$" entry))))

(ert-deftest task-conductor-project-max-name-width ()
  ;; Max width is computed from the longest name.
  (should (= 8 (task-conductor-project--max-name-width
                (list (list :project/name "ab")
                      (list :project/name "longname"))))))

(ert-deftest task-conductor-project-render-section-values ()
  ;; Each project entry section stores the project plist as its value.
  (with-project-buffer
    (let* ((proj (list :project/name "test" :project/path "/t"))
           (projects (list proj)))
      (task-conductor-project--render projects)
      (goto-char (point-min))
      ;; Navigate into the first entry section
      (magit-section-forward)
      (let ((section (magit-current-section)))
        (should (eq (oref section type) 'task-conductor-project-entry))
        (should (equal "test" (plist-get (oref section value)
                                         :project/name)))))))

(provide 'task-conductor-project-test)
;;; task-conductor-project-test.el ends here
