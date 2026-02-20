;;; task-conductor-project.el --- Project management for task-conductor -*- lexical-binding: t; -*-

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

;; Interactive Emacs buffer for managing projects registered in the
;; task-conductor JVM project registry.  Uses magit-section for a
;; collapsible, navigable UI.
;;
;; Usage:
;;   M-x task-conductor-project-list
;;
;; Key bindings:
;;   g   - Refresh project list from JVM
;;   c   - Create a new project
;;   d   - Delete project at point
;;   r   - Rename project at point
;;   RET - Open project directory in dired
;;   q   - Quit buffer

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'task-conductor-dev-env)

;;; Project CRUD

(defun task-conductor-project--eval-or-error (form)
  "Evaluate Clojure FORM, returning result plist or an error plist.
Checks dev-env connection first.  On nREPL failure or nil result,
returns (:status :error :message ...)."
  (unless (task-conductor-dev-env--connected-p)
    (user-error "Not connected to task-conductor"))
  (condition-case err
      (or (task-conductor-dev-env--eval-sync form)
          (list :status :error :message "No result from nREPL"))
    (error (list :status :error :message (error-message-string err)))))

(defun task-conductor-project--list ()
  "Query all registered projects from the JVM.
Returns a plist with :status and :projects on success,
or :status :error and :message on failure."
  (task-conductor-project--eval-or-error
   (format "(task-conductor.emacs-dev-env.interface/query-projects-by-id %S)"
           task-conductor-dev-env--dev-env-id)))

(defun task-conductor-project--create (path &optional name)
  "Create a project at PATH with optional NAME.
Returns a plist with :status and :project on success,
or :status :error and :message on failure."
  (task-conductor-project--eval-or-error
   (format "(task-conductor.emacs-dev-env.interface/create-project-by-id %S %S %S)"
           task-conductor-dev-env--dev-env-id
           path
           name)))

(defun task-conductor-project--update (path name)
  "Update project at PATH to have NAME.
Returns a plist with :status and :project on success,
or :status :error and :message on failure."
  (task-conductor-project--eval-or-error
   (format "(task-conductor.emacs-dev-env.interface/update-project-by-id %S %S %S)"
           task-conductor-dev-env--dev-env-id
           path
           name)))

(defun task-conductor-project--delete (path)
  "Delete project at PATH.
Returns a plist with :status and :project on success,
or :status :error and :message on failure."
  (task-conductor-project--eval-or-error
   (format "(task-conductor.emacs-dev-env.interface/delete-project-by-id %S %S)"
           task-conductor-dev-env--dev-env-id
           path)))

;;; Buffer state

(defvar task-conductor-project--buffer-name "*task-conductor-projects*"
  "Name of the projects buffer.")

;;; Section types

(defclass task-conductor-project-root-section (magit-section) ())
(defclass task-conductor-project-entry-section (magit-section) ())

;;; Rendering

(defun task-conductor-project--max-name-width (projects)
  "Return the maximum name width across PROJECTS."
  (cl-loop for p in projects
           maximize (length (or (plist-get p :project/name) ""))))

(defun task-conductor-project--status-icon (status)
  "Return a status icon string for STATUS keyword."
  (pcase status
    (:running "⚡")
    (:escalated "⚡")
    (:idle "⏸")
    (_ " ")))

(defun task-conductor-project--status-info (project)
  "Return a status info string for PROJECT, or nil."
  (when-let ((sessions (plist-get project :project/active-sessions)))
    (let* ((first-session (car sessions))
           (state (plist-get first-session :state))
           (task-id (plist-get first-session :task-id))
           (count (length sessions)))
      (if (> count 1)
          (format "[%s: task %s +%d]" state task-id (1- count))
        (format "[%s: task %s]" state task-id)))))

(defun task-conductor-project--format-entry (project name-width)
  "Format PROJECT entry with NAME-WIDTH padding and status."
  (let* ((name (or (plist-get project :project/name) "unnamed"))
         (path (or (plist-get project :project/path) ""))
         (status (plist-get project :project/status))
         (icon (task-conductor-project--status-icon status))
         (padded (concat name (make-string (max 0 (- name-width (length name))) ?\s)))
         (info (task-conductor-project--status-info project)))
    (if info
        (format "%s %s  %s    %s" icon padded path info)
      (format "%s %s  %s" icon padded path))))

(defun task-conductor-project--render (projects)
  "Render PROJECTS into the current buffer.
PROJECTS is a list of plists with :project/name and :project/path."
  (let ((inhibit-read-only t)
        (saved-section-ident (when-let ((s (magit-current-section)))
                               (magit-section-ident s)))
        (name-width (if projects
                        (task-conductor-project--max-name-width projects)
                      0)))
    (erase-buffer)
    (magit-insert-section (task-conductor-project-root)
      (magit-insert-heading (format "Projects (%d)\n" (length projects)))
      (if projects
          (dolist (p projects)
            (magit-insert-section (task-conductor-project-entry p)
              (magit-insert-heading
                (format "  %s\n"
                        (task-conductor-project--format-entry p name-width)))))
        (insert "  (none)\n")))
    (if saved-section-ident
        (when-let ((s (magit-get-section saved-section-ident)))
          (goto-char (oref s start)))
      (goto-char (point-min)))))

;;; Auto-refresh

(defun task-conductor-project-rerender-if-live ()
  "Re-render projects buffer from cached data if it exists.
Called by the :notify-projects-changed handler."
  (when-let ((buf (get-buffer task-conductor-project--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (task-conductor-project--render
         task-conductor-dev-env--cached-projects)))))

;;; Interactive commands

(defun task-conductor-project-refresh ()
  "Refresh the project list from the JVM."
  (interactive)
  (if (not (task-conductor-dev-env--connected-p))
      (progn
        (task-conductor-project--render nil)
        (message "Not connected to task-conductor"))
    (let ((result (task-conductor-project--list)))
      (if (eq :ok (plist-get result :status))
          (let ((projects (plist-get result :projects)))
            (setq task-conductor-dev-env--cached-projects projects)
            (task-conductor-project--render projects)
            (message "Projects refreshed"))
        (message "Error fetching projects: %s"
                 (or (plist-get result :message) "unknown"))))))

(defun task-conductor-project-create ()
  "Create a new project.
Prompts for directory path and optional name."
  (interactive)
  (let* ((path (read-directory-name "Project path: "))
         (name (read-string "Project name (empty for dir name): ")))
    (when (string-empty-p name)
      (setq name nil))
    (let ((result (task-conductor-project--create path name)))
      (if (eq :ok (plist-get result :status))
          (progn
            (message "Created project at %s" path)
            (task-conductor-project-refresh))
        (message "Error creating project: %s"
                 (or (plist-get result :message) "unknown"))))))

(defun task-conductor-project-delete ()
  "Delete the project at point."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'task-conductor-project-entry)
      (let* ((project (oref section value))
             (path (plist-get project :project/path))
             (name (or (plist-get project :project/name) path)))
        (when (y-or-n-p (format "Delete project %s? " name))
          (let ((result (task-conductor-project--delete path)))
            (if (eq :ok (plist-get result :status))
                (progn
                  (message "Deleted project %s" name)
                  (task-conductor-project-refresh))
              (message "Error deleting project: %s"
                       (or (plist-get result :message) "unknown")))))))))

(defun task-conductor-project-rename ()
  "Rename the project at point."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'task-conductor-project-entry)
      (let* ((project (oref section value))
             (path (plist-get project :project/path))
             (old-name (or (plist-get project :project/name) ""))
             (new-name (read-string "New name: " old-name)))
        (unless (string-empty-p new-name)
          (let ((result (task-conductor-project--update path new-name)))
            (if (eq :ok (plist-get result :status))
                (progn
                  (message "Renamed to %s" new-name)
                  (task-conductor-project-refresh))
              (message "Error renaming project: %s"
                       (or (plist-get result :message) "unknown")))))))))

(defun task-conductor-project-open-dired ()
  "Open the project directory at point in dired."
  (interactive)
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'task-conductor-project-entry)
      (let ((path (plist-get (oref section value) :project/path)))
        (if (and path (file-directory-p path))
            (dired path)
          (message "Directory not found: %s" (or path "?")))))))

(defun task-conductor-project-quit ()
  "Quit the projects buffer."
  (interactive)
  (quit-window))

;;; Mode

(defvar task-conductor-project-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map magit-section-mode-map)
    (define-key map (kbd "g") #'task-conductor-project-refresh)
    (define-key map (kbd "c") #'task-conductor-project-create)
    (define-key map (kbd "d") #'task-conductor-project-delete)
    (define-key map (kbd "r") #'task-conductor-project-rename)
    (define-key map (kbd "RET") #'task-conductor-project-open-dired)
    (define-key map (kbd "q") #'task-conductor-project-quit)
    map)
  "Keymap for `task-conductor-project-mode'.")

(define-derived-mode task-conductor-project-mode magit-section-mode
  "TC Projects"
  "Major mode for managing task-conductor projects.

\\{task-conductor-project-mode-map}")

;;; Entry point

;;;###autoload
(defun task-conductor-project-list ()
  "Open the task-conductor projects buffer."
  (interactive)
  (let ((buf (get-buffer-create task-conductor-project--buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'task-conductor-project-mode)
        (task-conductor-project-mode))
      (task-conductor-project-refresh))
    (pop-to-buffer buf)))

(provide 'task-conductor-project)
;;; task-conductor-project.el ends here
