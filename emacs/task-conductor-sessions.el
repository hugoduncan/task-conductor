;;; task-conductor-sessions.el --- Session viewer for task-conductor -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: task-conductor contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (magit-section "4.0.0"))
;; Keywords: tools, processes
;; URL: https://github.com/hugoduncan/task-conductor

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Displays Claude sessions grouped by attention priority: needs
;; attention (escalated+idle), running (escalated+running), idle, and
;; PR-waiting.  Uses magit-section for a collapsible, navigable UI.  Session data
;; comes from the JVM via the dev-env mechanism and is refreshed
;; automatically while the buffer is visible.
;;
;; Usage:
;;   M-x task-conductor-sessions
;;
;; Key bindings:
;;   RET - Switch to session's *claude:* buffer
;;   m   - Trigger PR merge for :wait-pr-merge session at point
;;   g   - Refresh session list from JVM
;;   q   - Quit session viewer

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'task-conductor-dev-env)

;;; Customization

(defgroup task-conductor-sessions nil
  "Session viewer for task-conductor."
  :group 'task-conductor-dev-env
  :prefix "task-conductor-sessions-")

(defcustom task-conductor-sessions-refresh-interval 5
  "Auto-refresh interval in seconds while buffer is visible."
  :type 'integer
  :group 'task-conductor-sessions)

;;; State

(defvar task-conductor-sessions--refresh-timer nil
  "Timer for auto-refreshing the sessions buffer.")

(defconst task-conductor-sessions--buffer-name "*task-conductor-sessions*"
  "Name of the sessions buffer.")

;;; Section types

(defclass task-conductor-sessions-root-section (magit-section) ())
(defclass task-conductor-sessions-group-section (magit-section) ())
(defclass task-conductor-sessions-entry-section (magit-section) ())

;;; Time formatting

(defun task-conductor-sessions--format-relative-time (timestamp)
  "Format TIMESTAMP as relative time string.
TIMESTAMP is an ISO-8601 string or epoch seconds number.
Returns a string like \"3m ago\", \"1h ago\", \"2d ago\"."
  (when timestamp
    (let* ((time (cond
                  ((numberp timestamp) (seconds-to-time timestamp))
                  ((stringp timestamp) (date-to-time timestamp))
                  (t nil)))
           (elapsed (and time (float-time (time-subtract nil time)))))
      (cond
       ((null elapsed) "?")
       ((< elapsed 60) (format "%ds ago" (truncate elapsed)))
       ((< elapsed 3600) (format "%dm ago" (truncate (/ elapsed 60))))
       ((< elapsed 86400) (format "%dh ago" (truncate (/ elapsed 3600))))
       (t (format "%dd ago" (truncate (/ elapsed 86400))))))))

;;; Rendering

(defun task-conductor-sessions--wait-pr-merge-p (state)
  "Return non-nil if STATE represents the :wait-pr-merge session state."
  (or (eq state :wait-pr-merge) (equal state "wait-pr-merge")))

(defun task-conductor-sessions--state-icon (state &optional sub-state)
  "Return icon string for session STATE keyword.
When STATE is escalated, SUB-STATE distinguishes idle from running."
  (pcase state
    ((or :escalated "escalated")
     (pcase sub-state
       ((or :session-running "session-running") "🔄")
       (_ "🔔")))
    ((or :idle "idle") "⏸")
    ((or :wait-pr-merge "wait-pr-merge") "🔀")
    (_ "?")))

(defun task-conductor-sessions--format-session-heading (session)
  "Format heading string for SESSION plist.
Shows PR number and branch for :wait-pr-merge sessions."
  (let ((task-id (plist-get session :task-id))
        (task-title (or (plist-get session :task-title) "untitled"))
        (state (plist-get session :state))
        (sub-state (plist-get session :sub-state))
        (entered (plist-get session :entered-state-at))
        (pr-num (plist-get session :pr-num))
        (branch (plist-get session :branch)))
    (format "%s #%s %s%s  %s"
            (task-conductor-sessions--state-icon state sub-state)
            (if task-id (format "%s" task-id) "?")
            task-title
            (if (and (task-conductor-sessions--wait-pr-merge-p state)
                     (or pr-num branch))
                (format "  %s%s"
                        (if pr-num (format "PR #%s" pr-num) "")
                        (if branch (format " %s" branch) ""))
              "")
            (task-conductor-sessions--format-relative-time entered))))

(defun task-conductor-sessions--session-running-p (sub-state)
  "Return non-nil if SUB-STATE indicates the session is running."
  (or (eq sub-state :session-running) (equal sub-state "session-running")))

(defun task-conductor-sessions--partition-by-state (sessions)
  "Partition SESSIONS into groups by state.
Returns a plist with keys :needs-attention :running :idle :wait-pr-merge.
Escalated sessions are split by :sub-state — :session-running goes to
:running, everything else (including nil) to :needs-attention."
  (let (needs-attention running idle wait-pr-merge)
    (dolist (s sessions)
      (let ((state (plist-get s :state)))
        (cond
         ((or (eq state :escalated) (equal state "escalated"))
          (if (task-conductor-sessions--session-running-p
               (plist-get s :sub-state))
              (push s running)
            (push s needs-attention)))
         ((or (eq state :idle) (equal state "idle"))
          (push s idle))
         ((task-conductor-sessions--wait-pr-merge-p state)
          (push s wait-pr-merge))
         (t
          (message "task-conductor-sessions: unknown state %S in session %S"
                   state (plist-get s :session-id))))))
    (list :needs-attention (nreverse needs-attention)
          :running (nreverse running)
          :idle (nreverse idle)
          :wait-pr-merge (nreverse wait-pr-merge))))

(defun task-conductor-sessions--insert-session-entry (session)
  "Insert a magit-section entry for SESSION plist."
  (magit-insert-section (task-conductor-sessions-entry session)
    (magit-insert-heading
      (format "  %s\n" (task-conductor-sessions--format-session-heading session)))))

(defun task-conductor-sessions--insert-group (label sessions)
  "Insert a group section with LABEL containing SESSIONS."
  (magit-insert-section (task-conductor-sessions-group label)
    (magit-insert-heading (format "%s (%d)\n" label (length sessions)))
    (if sessions
        (dolist (s sessions)
          (task-conductor-sessions--insert-session-entry s))
      (insert "  (none)\n"))))

(defun task-conductor-sessions--render (sessions)
  "Render SESSIONS into the current buffer.
SESSIONS is a list of plists with :session-id, :state, :task-id,
:task-title, :entered-state-at, :pr-num, :branch."
  (let ((inhibit-read-only t)
        (parts (task-conductor-sessions--partition-by-state sessions)))
    (erase-buffer)
    (magit-insert-section (task-conductor-sessions-root)
      (magit-insert-heading "Claude Sessions\n")
      (task-conductor-sessions--insert-group "Needs Attention" (plist-get parts :needs-attention))
      (task-conductor-sessions--insert-group "Running" (plist-get parts :running))
      (task-conductor-sessions--insert-group "Idle" (plist-get parts :idle))
      (task-conductor-sessions--insert-group "PR Waiting" (plist-get parts :wait-pr-merge))))
  (goto-char (point-min)))

;;; Actions

(defun task-conductor-sessions-goto-session ()
  "Switch to the *claude:* buffer for the session at point."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when-let ((session (and (eq (oref section type)
                                 'task-conductor-sessions-entry)
                             (oref section value))))
      (let* ((session-id (plist-get session :session-id))
             (buffer (and session-id
                          (gethash session-id
                                   task-conductor-dev-env--sessions))))
        (if (and buffer (buffer-live-p buffer))
            (pop-to-buffer buffer)
          (message "No buffer for session %s" (or session-id "?")))))))

(defun task-conductor-sessions--refresh ()
  "Query JVM for sessions and re-render.  No user-facing message."
  (when-let ((buf (get-buffer task-conductor-sessions--buffer-name)))
    (with-current-buffer buf
      (if (not (task-conductor-dev-env--connected-p))
          (task-conductor-sessions--render nil)
        (task-conductor-dev-env-query-sessions)
        (task-conductor-sessions--render task-conductor-dev-env--cached-sessions)))))

(defun task-conductor-sessions-refresh ()
  "Refresh session list from JVM and re-render."
  (interactive)
  (task-conductor-sessions--refresh)
  (if (task-conductor-dev-env--connected-p)
      (message "Sessions refreshed")
    (message "Not connected to task-conductor")))

(defun task-conductor-sessions-merge-pr ()
  "Trigger PR merge for the :wait-pr-merge session at point."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when-let ((session (and (eq (oref section type)
                                 'task-conductor-sessions-entry)
                             (oref section value))))
      (let ((state (plist-get session :state))
            (session-id (plist-get session :session-id)))
        (unless session-id
          (user-error "Session has no session-id"))
        (unless (task-conductor-sessions--wait-pr-merge-p state)
          (user-error "Session is not in :wait-pr-merge state"))
        (let ((result (task-conductor-dev-env--eval-sync
                       (format "(let [r (task-conductor.pathom-graph.interface/query [`(task-conductor.project.resolvers/pr-merge! {:engine/session-id %S})])] (get r 'task-conductor.project.resolvers/pr-merge!))"
                               session-id))))
          (if (eq :triggered (plist-get result :pr-merge/status))
              (message "PR merge triggered for session %s" session-id)
            (message "PR merge failed: %s"
                     (plist-get (plist-get result :pr-merge/error) :message))))))))

(defun task-conductor-sessions-quit ()
  "Quit the sessions buffer."
  (interactive)
  (quit-window))

;;; Auto-refresh timer

(defun task-conductor-sessions--buffer-visible-p ()
  "Return non-nil if the sessions buffer is visible in a window."
  (when-let ((buf (get-buffer task-conductor-sessions--buffer-name)))
    (get-buffer-window buf t)))

(defun task-conductor-sessions--auto-refresh ()
  "Auto-refresh callback.  Only refreshes when buffer is visible."
  (when (task-conductor-sessions--buffer-visible-p)
    (with-current-buffer (get-buffer task-conductor-sessions--buffer-name)
      (condition-case err
          (task-conductor-sessions--refresh)
        (error
         (message "task-conductor-sessions: auto-refresh error: %s"
                  (error-message-string err)))))))

(defun task-conductor-sessions--start-timer ()
  "Start the auto-refresh timer."
  (unless task-conductor-sessions--refresh-timer
    (setq task-conductor-sessions--refresh-timer
          (run-with-timer task-conductor-sessions-refresh-interval
                          task-conductor-sessions-refresh-interval
                          #'task-conductor-sessions--auto-refresh))))

(defun task-conductor-sessions--stop-timer ()
  "Stop the auto-refresh timer."
  (when task-conductor-sessions--refresh-timer
    (cancel-timer task-conductor-sessions--refresh-timer)
    (setq task-conductor-sessions--refresh-timer nil)))

(defun task-conductor-sessions--on-window-change ()
  "Start or stop the auto-refresh timer based on buffer visibility.
Added to `window-configuration-change-hook' as a buffer-local hook."
  (if (task-conductor-sessions--buffer-visible-p)
      (task-conductor-sessions--start-timer)
    (task-conductor-sessions--stop-timer)))

(defun task-conductor-sessions--kill-buffer-hook ()
  "Clean up when sessions buffer is killed."
  (task-conductor-sessions--stop-timer))

;;; Re-render on push notification

(defun task-conductor-sessions-rerender-if-live ()
  "Re-render sessions buffer from cached data if it exists.
Called by the :notify-sessions-changed handler."
  (when-let ((buf (get-buffer task-conductor-sessions--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (task-conductor-sessions--render
         task-conductor-dev-env--cached-sessions)))))

;;; Mode

(defvar-keymap task-conductor-sessions-mode-map
  :doc "Keymap for `task-conductor-sessions-mode'."
  :parent magit-section-mode-map
  "RET" #'task-conductor-sessions-goto-session
  "g"   #'task-conductor-sessions-refresh
  "m"   #'task-conductor-sessions-merge-pr
  "q"   #'task-conductor-sessions-quit)

(define-derived-mode task-conductor-sessions-mode magit-section-mode
  "TC Sessions"
  "Major mode for viewing task-conductor Claude sessions.

\\{task-conductor-sessions-mode-map}"
  :group 'task-conductor-sessions
  (add-hook 'kill-buffer-hook
            #'task-conductor-sessions--kill-buffer-hook nil t)
  (add-hook 'window-configuration-change-hook
            #'task-conductor-sessions--on-window-change nil t))

;;; Entry point

;;;###autoload
(defun task-conductor-sessions ()
  "Open the task-conductor sessions buffer.
Shows Claude sessions in escalated or idle states."
  (interactive)
  (let ((buf (get-buffer-create task-conductor-sessions--buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'task-conductor-sessions-mode)
        (task-conductor-sessions-mode))
      (task-conductor-sessions-refresh)
      (task-conductor-sessions--start-timer))
    (pop-to-buffer buf)))

(provide 'task-conductor-sessions)
;;; task-conductor-sessions.el ends here
