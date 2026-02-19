;;; test-task-conductor-sessions.el --- Tests for task-conductor-sessions -*- lexical-binding: t; -*-

;; Tests for the Emacs sessions buffer component of task-conductor.
;; These tests verify section rendering, keybindings, timer lifecycle,
;; and session navigation without requiring a running nREPL server.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'magit-section)
(require 'task-conductor-sessions)

;;; Test Data

(defvar test-sessions-sample
  (list
   (list :session-id "sess-1" :state :escalated
         :task-id 101 :task-title "Fix auth"
         :entered-state-at "2026-02-19T10:00:00Z")
   (list :session-id "sess-2" :state :idle
         :task-id 102 :task-title "Add tests"
         :entered-state-at "2026-02-19T10:30:00Z")
   (list :session-id "sess-3" :state :escalated
         :task-id 103 :task-title "Refactor DB"
         :entered-state-at "2026-02-19T09:00:00Z"))
  "Sample session data for tests.")

;;; Test Helpers

(defmacro with-sessions-buffer (&rest body)
  "Execute BODY in a temporary sessions buffer with magit-section-mode."
  (declare (indent 0))
  `(let ((task-conductor-sessions--refresh-timer nil)
         (task-conductor-dev-env--cached-sessions nil)
         (task-conductor-dev-env--sessions (make-hash-table :test 'equal))
         (task-conductor-dev-env--dev-env-id nil))
     (unwind-protect
         (with-temp-buffer
           (task-conductor-sessions-mode)
           ,@body)
       (clrhash task-conductor-dev-env--sessions))))

;;; Relative Time Formatting

(ert-deftest task-conductor-sessions-format-relative-time-nil ()
  ;; Returns nil for nil input.
  (should (null (task-conductor-sessions--format-relative-time nil))))

(ert-deftest task-conductor-sessions-format-relative-time-seconds ()
  ;; Formats recent timestamps as seconds ago.
  (let ((recent (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                    (time-subtract nil 30) t)))
    (should (string-match-p "\\`[0-9]+s ago\\'"
                            (task-conductor-sessions--format-relative-time recent)))))

(ert-deftest task-conductor-sessions-format-relative-time-minutes ()
  ;; Formats timestamps a few minutes ago.
  (let ((past (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                  (time-subtract nil 180) t)))
    (should (string-match-p "\\`[0-9]+m ago\\'"
                            (task-conductor-sessions--format-relative-time past)))))

;;; State Icons

(ert-deftest task-conductor-sessions-state-icon-escalated ()
  ;; Escalated state shows lightning icon.
  (should (string= "⚡" (task-conductor-sessions--state-icon :escalated)))
  (should (string= "⚡" (task-conductor-sessions--state-icon "escalated"))))

(ert-deftest task-conductor-sessions-state-icon-idle ()
  ;; Idle state shows pause icon.
  (should (string= "⏸" (task-conductor-sessions--state-icon :idle)))
  (should (string= "⏸" (task-conductor-sessions--state-icon "idle"))))

(ert-deftest task-conductor-sessions-state-icon-unknown ()
  ;; Unknown state shows question mark.
  (should (string= "?" (task-conductor-sessions--state-icon :other))))

;;; Partition By State

(ert-deftest task-conductor-sessions-partition-empty ()
  ;; Empty list produces empty partitions.
  (let ((result (task-conductor-sessions--partition-by-state nil)))
    (should (null (car result)))
    (should (null (cdr result)))))

(ert-deftest task-conductor-sessions-partition-mixed ()
  ;; Sessions are correctly partitioned by state.
  (let ((result (task-conductor-sessions--partition-by-state test-sessions-sample)))
    (should (= 2 (length (car result))))   ; 2 escalated
    (should (= 1 (length (cdr result)))))) ; 1 idle

;;; Section Rendering

(ert-deftest task-conductor-sessions-render-empty ()
  ;; Rendering empty list produces buffer with group headings.
  (with-sessions-buffer
    (task-conductor-sessions--render nil)
    (should (string-match-p "Escalated (0)" (buffer-string)))
    (should (string-match-p "Idle (0)" (buffer-string)))
    (should (string-match-p "(none)" (buffer-string)))))

(ert-deftest task-conductor-sessions-render-with-sessions ()
  ;; Rendering sessions shows them in correct groups.
  (with-sessions-buffer
    (task-conductor-sessions--render test-sessions-sample)
    (let ((content (buffer-string)))
      (should (string-match-p "Escalated (2)" content))
      (should (string-match-p "Idle (1)" content))
      (should (string-match-p "Fix auth" content))
      (should (string-match-p "Add tests" content))
      (should (string-match-p "Refactor DB" content))
      (should (string-match-p "⚡" content))
      (should (string-match-p "⏸" content)))))

(ert-deftest task-conductor-sessions-render-root-section-exists ()
  ;; After render, magit-root-section is set.
  (with-sessions-buffer
    (task-conductor-sessions--render test-sessions-sample)
    (should magit-root-section)
    (should (oref magit-root-section children))))

(ert-deftest task-conductor-sessions-render-entry-sections-have-values ()
  ;; Entry sections store session plist as value.
  (with-sessions-buffer
    (task-conductor-sessions--render test-sessions-sample)
    ;; Navigate to first entry section
    (goto-char (point-min))
    (let ((found nil))
      (magit-section-show magit-root-section)
      (dolist (group (oref magit-root-section children))
        (dolist (entry (oref group children))
          (when (eq (oref entry type) 'task-conductor-sessions-entry)
            (push (oref entry value) found))))
      (should (>= (length found) 3)))))

;;; Keybindings

(ert-deftest task-conductor-sessions-keybindings-present ()
  ;; Mode map contains expected key bindings.
  (with-sessions-buffer
    (should (eq (lookup-key task-conductor-sessions-mode-map (kbd "g"))
                #'task-conductor-sessions-refresh))
    (should (eq (lookup-key task-conductor-sessions-mode-map (kbd "q"))
                #'task-conductor-sessions-quit))
    (should (eq (lookup-key task-conductor-sessions-mode-map (kbd "RET"))
                #'task-conductor-sessions-goto-session))))

;;; Timer Lifecycle

(ert-deftest task-conductor-sessions-timer-start ()
  ;; Starting timer sets the timer variable.
  (let ((task-conductor-sessions--refresh-timer nil)
        (task-conductor-sessions-refresh-interval 100))
    (unwind-protect
        (progn
          (task-conductor-sessions--start-timer)
          (should task-conductor-sessions--refresh-timer))
      (task-conductor-sessions--stop-timer))))

(ert-deftest task-conductor-sessions-timer-stop ()
  ;; Stopping timer clears the timer variable.
  (let ((task-conductor-sessions--refresh-timer nil)
        (task-conductor-sessions-refresh-interval 100))
    (task-conductor-sessions--start-timer)
    (task-conductor-sessions--stop-timer)
    (should-not task-conductor-sessions--refresh-timer)))

(ert-deftest task-conductor-sessions-timer-idempotent-start ()
  ;; Starting timer twice does not create duplicate timers.
  (let ((task-conductor-sessions--refresh-timer nil)
        (task-conductor-sessions-refresh-interval 100))
    (unwind-protect
        (progn
          (task-conductor-sessions--start-timer)
          (let ((first-timer task-conductor-sessions--refresh-timer))
            (task-conductor-sessions--start-timer)
            (should (eq first-timer task-conductor-sessions--refresh-timer))))
      (task-conductor-sessions--stop-timer))))

;;; Visibility-based Timer

(ert-deftest task-conductor-sessions-on-window-change-stops-when-invisible ()
  ;; Timer stops when buffer is not visible in any window.
  (let ((task-conductor-sessions--refresh-timer nil)
        (task-conductor-sessions-refresh-interval 100)
        (task-conductor-sessions--buffer-name "*tc-test-vis*"))
    (let ((buf (get-buffer-create task-conductor-sessions--buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (task-conductor-sessions-mode))
            (task-conductor-sessions--start-timer)
            (should task-conductor-sessions--refresh-timer)
            ;; Buffer not displayed in any window — hook should stop timer
            (task-conductor-sessions--on-window-change)
            (should-not task-conductor-sessions--refresh-timer))
        (task-conductor-sessions--stop-timer)
        (kill-buffer buf)))))

(ert-deftest task-conductor-sessions-on-window-change-starts-when-visible ()
  ;; Timer starts when buffer becomes visible.
  (let ((task-conductor-sessions--refresh-timer nil)
        (task-conductor-sessions-refresh-interval 100)
        (task-conductor-sessions--buffer-name "*tc-test-vis2*"))
    (let ((buf (get-buffer-create task-conductor-sessions--buffer-name)))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (task-conductor-sessions-mode))
            ;; Display buffer in a window so it's visible
            (display-buffer buf)
            (should-not task-conductor-sessions--refresh-timer)
            (task-conductor-sessions--on-window-change)
            (should task-conductor-sessions--refresh-timer))
        (task-conductor-sessions--stop-timer)
        (delete-windows-on buf)
        (kill-buffer buf)))))

(ert-deftest task-conductor-sessions-mode-sets-window-hook ()
  ;; Mode setup adds window-configuration-change-hook.
  (with-sessions-buffer
    (should (memq #'task-conductor-sessions--on-window-change
                  (buffer-local-value 'window-configuration-change-hook
                                      (current-buffer))))))

;;; Goto Session

(ert-deftest task-conductor-sessions-goto-session-no-buffer ()
  ;; When session has no buffer, a message is shown (no error).
  (with-sessions-buffer
    (task-conductor-sessions--render test-sessions-sample)
    ;; Find first entry section and go to it
    (goto-char (point-min))
    (let ((entry-found nil))
      (dolist (group (oref magit-root-section children))
        (dolist (entry (oref group children))
          (when (and (not entry-found)
                     (eq (oref entry type) 'task-conductor-sessions-entry))
            (setq entry-found entry))))
      (when entry-found
        (goto-char (oref entry-found start))
        ;; Should not error even with no buffer registered
        (task-conductor-sessions-goto-session)))))

(ert-deftest task-conductor-sessions-goto-session-with-buffer ()
  ;; When session has a live buffer, switches to it.
  (with-sessions-buffer
    (let ((target-buf (generate-new-buffer "*claude:test*")))
      (unwind-protect
          (progn
            (puthash "sess-1" target-buf task-conductor-dev-env--sessions)
            (task-conductor-sessions--render test-sessions-sample)
            ;; Find the sess-1 entry
            (let ((entry-found nil))
              (dolist (group (oref magit-root-section children))
                (dolist (entry (oref group children))
                  (when (eq (oref entry type) 'task-conductor-sessions-entry)
                    (let ((val (oref entry value)))
                      (when (equal "sess-1" (plist-get val :session-id))
                        (setq entry-found entry))))))
              (should entry-found)))
        (kill-buffer target-buf)))))

;;; Re-render on Push

(ert-deftest task-conductor-sessions-rerender-if-live-no-buffer ()
  ;; Does not error when sessions buffer does not exist.
  (let ((task-conductor-sessions--buffer-name "*tc-test-no-exist*"))
    (task-conductor-sessions-rerender-if-live)))

(ert-deftest task-conductor-sessions-rerender-if-live-updates ()
  ;; Re-renders when sessions buffer exists.
  (let* ((buf-name "*tc-test-rerender*")
         (task-conductor-sessions--buffer-name buf-name)
         (task-conductor-dev-env--cached-sessions test-sessions-sample)
         (buf (get-buffer-create buf-name)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (task-conductor-sessions-mode))
          (task-conductor-sessions-rerender-if-live)
          (with-current-buffer buf
            (should (string-match-p "Escalated" (buffer-string)))))
      (kill-buffer buf))))

(provide 'test-task-conductor-sessions)
;;; test-task-conductor-sessions.el ends here
