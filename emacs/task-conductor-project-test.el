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
  ;; Format pads name to the given width with status icon prefix.
  (let ((entry (task-conductor-project--format-entry
                (list :project/name "ab" :project/path "/x")
                10)))
    (should (string-match-p "ab " entry))
    (should (string-match-p "/x" entry))))

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

;;; Status display tests

(ert-deftest task-conductor-project-status-icon ()
  ;; Status icons map correctly.
  (should (equal "⚡" (task-conductor-project--status-icon :running)))
  (should (equal "⚡" (task-conductor-project--status-icon :escalated)))
  (should (equal "⏸" (task-conductor-project--status-icon :idle)))
  (should (equal " " (task-conductor-project--status-icon nil))))

(ert-deftest task-conductor-project-status-info-single ()
  ;; Status info shows state and task ID for a single session.
  (let* ((sessions (list (list :state :running :task-id 42)))
         (project (list :project/name "p" :project/path "/p"
                        :project/status :running
                        :project/active-sessions sessions))
         (info (task-conductor-project--status-info project)))
    (should (string-match-p "running" info))
    (should (string-match-p "task 42" info))))

(ert-deftest task-conductor-project-status-info-multiple ()
  ;; Status info shows count for multiple sessions.
  (let* ((sessions (list (list :state :running :task-id 1)
                         (list :state :idle :task-id 2)))
         (project (list :project/name "p" :project/path "/p"
                        :project/active-sessions sessions))
         (info (task-conductor-project--status-info project)))
    (should (string-match-p "\\+1" info))))

(ert-deftest task-conductor-project-status-info-nil ()
  ;; No status info when no sessions.
  (let ((project (list :project/name "p" :project/path "/p")))
    (should-not (task-conductor-project--status-info project))))

(ert-deftest task-conductor-project-format-entry-with-status ()
  ;; Format includes status icon and session info.
  (let* ((sessions (list (list :state :running :task-id 5)))
         (project (list :project/name "proj" :project/path "/proj"
                        :project/status :running
                        :project/active-sessions sessions))
         (entry (task-conductor-project--format-entry project 10)))
    (should (string-match-p "⚡" entry))
    (should (string-match-p "running: task 5" entry))))

(ert-deftest task-conductor-project-format-entry-no-status ()
  ;; Format shows space icon when no status.
  (let* ((project (list :project/name "proj" :project/path "/proj"))
         (entry (task-conductor-project--format-entry project 10)))
    (should-not (string-match-p "⚡" entry))
    (should-not (string-match-p "⏸" entry))))

(ert-deftest task-conductor-project-rerender-if-live ()
  ;; Rerender updates buffer from cached projects when buffer exists.
  (let* ((buf-name "*tc-test-proj-rerender*")
         (task-conductor-project--buffer-name buf-name)
         (task-conductor-dev-env--cached-projects
          (list (list :project/name "cached" :project/path "/cached")))
         (buf (get-buffer-create buf-name)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (task-conductor-project-mode))
          (task-conductor-project-rerender-if-live)
          (with-current-buffer buf
            (should (string-match-p "cached" (buffer-string)))
            (should (string-match-p "Projects (1)" (buffer-string)))))
      (kill-buffer buf))))

;;; Error handling tests

(ert-deftest task-conductor-project-eval-or-error-nil-result ()
  ;; Returns error plist when eval-sync returns nil.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t))
            ((symbol-function 'task-conductor-dev-env--eval-sync)
             (lambda (_form) nil)))
    (let ((result (task-conductor-project--eval-or-error "(foo)")))
      (should (eq :error (plist-get result :status)))
      (should (plist-get result :message)))))

(ert-deftest task-conductor-project-eval-or-error-nrepl-error ()
  ;; Returns error plist when eval-sync signals an error.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t))
            ((symbol-function 'task-conductor-dev-env--eval-sync)
             (lambda (_form) (error "nREPL timeout"))))
    (let ((result (task-conductor-project--eval-or-error "(foo)")))
      (should (eq :error (plist-get result :status)))
      (should (string-match-p "timeout" (plist-get result :message))))))

(ert-deftest task-conductor-project-eval-or-error-success ()
  ;; Returns the eval-sync result when successful.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t))
            ((symbol-function 'task-conductor-dev-env--eval-sync)
             (lambda (_form) (list :status :ok :projects nil))))
    (let ((result (task-conductor-project--eval-or-error "(foo)")))
      (should (eq :ok (plist-get result :status))))))

(ert-deftest task-conductor-project-eval-or-error-not-connected ()
  ;; Signals user-error when not connected.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () nil)))
    (should-error (task-conductor-project--eval-or-error "(foo)")
                  :type 'user-error)))

(provide 'task-conductor-project-test)
;;; task-conductor-project-test.el ends here
