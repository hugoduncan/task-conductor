;;; task-conductor-project.el --- Project management for task-conductor -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: task-conductor contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (magit-section "4.0.0") (parseedn "1.0.0"))
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
;;   e   - Execute task or story at point
;;   k   - Cancel execution at point
;;   r   - Rename project at point
;;   RET - Open project directory in dired
;;   q   - Quit buffer

;;; Code:

(require 'cl-lib)
(require 'magit-section)
(require 'parseedn)
(require 'task-conductor-dev-env)

;;; CLI task fetching

(defun task-conductor-project--call-mcp-tasks-status (project-path status)
  "Call mcp-tasks CLI for STATUS tasks in PROJECT-PATH.
Returns output string on success (exit 0), nil on non-zero exit.
Signals an error if the CLI executable is not found."
  (let ((output-buffer (generate-new-buffer " *tc-mcp-tasks-out*")))
    (unwind-protect
        (let* ((default-directory project-path)
               (exit-code (call-process "mcp-tasks" nil output-buffer nil
                                        "list" "--status" status "--format" "edn")))
          (when (= exit-code 0)
            (with-current-buffer output-buffer (buffer-string))))
      (kill-buffer output-buffer))))

(defun task-conductor-project--parse-tasks-edn (edn-str)
  "Parse EDN-STR from mcp-tasks CLI output.
Expects {:tasks [...] :metadata {...}} map format.
Returns list of raw task plists (all fields), or nil on empty/failed parse.
parseedn returns hash-tables for maps and vectors for arrays."
  (when (and edn-str (not (string-empty-p (string-trim edn-str))))
    (condition-case nil
        (let* ((parsed (parseedn-read-str edn-str))
               (tasks-vec (when (hash-table-p parsed) (gethash :tasks parsed)))
               (raw-list (when (vectorp tasks-vec) (append tasks-vec nil))))
          (mapcar #'task-conductor-dev-env--edn-to-plist raw-list))
      (error nil))))

(defun task-conductor-project--fetch-tasks (project-path)
  "Fetch open and in-progress root tasks from mcp-tasks for PROJECT-PATH.
Calls mcp-tasks CLI directly; does not use the JVM nREPL connection.
Returns a list of task plists with :id, :title, :type, :status, :category,
sorted by :id ascending.  Root tasks are those without a :parent-id.
On any error, returns (:error \"message\")."
  (condition-case err
      (let* ((open-str (task-conductor-project--call-mcp-tasks-status
                        project-path "open"))
             (in-progress-str (task-conductor-project--call-mcp-tasks-status
                               project-path "in-progress"))
             (open-tasks (task-conductor-project--parse-tasks-edn open-str))
             (in-progress-tasks (task-conductor-project--parse-tasks-edn
                                 in-progress-str))
             (all-tasks (append open-tasks in-progress-tasks))
             (root-tasks (cl-remove-if
                          (lambda (task) (plist-get task :parent-id))
                          all-tasks))
             (seen (make-hash-table :test #'equal))
             (unique nil))
        (dolist (task root-tasks)
          (let ((id (plist-get task :id)))
            (unless (gethash id seen)
              (puthash id t seen)
              (push (list :id id
                          :title (plist-get task :title)
                          :type (plist-get task :type)
                          :status (plist-get task :status)
                          :category (plist-get task :category))
                    unique))))
        (sort (nreverse unique)
              (lambda (a b) (< (plist-get a :id) (plist-get b :id)))))
    (error (list :error (error-message-string err)))))

;;; Session lookup

(defun task-conductor-project--task-id-match-p (a b)
  "Return non-nil if task IDs A and B refer to the same task.
Handles numeric and string representations."
  (or (equal a b)
      (and (numberp a) (stringp b) (equal (number-to-string a) b))
      (and (stringp a) (numberp b) (equal a (number-to-string b)))))

(defun task-conductor-project--find-session (task-id &optional project-dir)
  "Find a session matching TASK-ID in cached session data.
Searches `task-conductor-dev-env--cached-sessions' for a session
whose :task-id matches TASK-ID.  When PROJECT-DIR is non-nil, also
filters by :project-dir.  Handles both numeric and string task-id
comparison.  Returns the session plist or nil if not found."
  (when task-id
    (cl-find-if
     (lambda (session)
       (and (task-conductor-project--task-id-match-p
             task-id (plist-get session :task-id))
            (or (null project-dir)
                (equal project-dir (plist-get session :project-dir)))))
     task-conductor-dev-env--cached-sessions)))

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

(defvar-local task-conductor-project--task-cache nil
  "Hash-table mapping project path strings to cached task lists.
Each value is either a list of task plists or (:error \"message\").
Nil until first use; initialized by `task-conductor-project-mode'.")

;;; Section types

(defclass task-conductor-project-root-section (magit-section) ())
(defclass task-conductor-project-entry-section (magit-section) ())
(defclass task-conductor-project-task-section (magit-section) ())

;;; Task formatting

(defvar task-conductor-project--play-icon-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'task-conductor-project-execute)
    map)
  "Keymap for the ▶ play icon on task lines without an active session.")

(defvar task-conductor-project--stop-icon-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] #'task-conductor-project-cancel)
    map)
  "Keymap for the ⏹ stop icon on task lines with a running or escalated session.")

(defun task-conductor-project--task-execution-icon (session-state)
  "Return an execution status icon for SESSION-STATE, or nil.
Maps session state keywords to unicode icons: idle, running,
escalated, and wait-pr-merge states each have a distinct icon.
Returns nil for unrecognized or nil states."
  (pcase session-state
    (:idle          "⏸")
    (:running       "🔄")
    (:escalated     "🔔")
    (:wait-pr-merge "🔀")
    (_              nil)))

(defun task-conductor-project--task-type-icon (type)
  "Return a bracketed type icon for task TYPE string."
  (pcase type
    ((or "task" :task)       "[T]")
    ((or "bug" :bug)         "[B]")
    ((or "feature" :feature) "[F]")
    ((or "story" :story)     "[S]")
    ((or "chore" :chore)     "[C]")
    (_                       "[?]")))

(defun task-conductor-project--task-status-icon (status)
  "Return a bracketed status icon for task STATUS string."
  (pcase status
    ((or "open" :open)                   "[ ]")
    ((or "in-progress" :in-progress)     "[>]")
    ((or "done" :done "closed" :closed)  "[x]")
    ((or "blocked" :blocked)             "[!]")
    (_                                   "[ ]")))

(defun task-conductor-project--format-task-entry (task &optional project-path)
  "Format TASK plist as a single display line.
Returns a string like `    ▶ [T][ ] #42 Some title'.
Icons appear at the start of the line after the indent.
When a session matches TASK's id, prepends an execution status icon.
For running or escalated sessions, also prepends a clickable ⏹ stop icon.
When no session is active, prepends a clickable ▶ play icon instead."
  (let* ((task-id (plist-get task :id))
         (indent "    ")
         (base (format "%s%s #%d %s"
                       (task-conductor-project--task-type-icon (plist-get task :type))
                       (task-conductor-project--task-status-icon (plist-get task :status))
                       task-id
                       (plist-get task :title)))
         (session (task-conductor-project--find-session task-id project-path))
         (state (when session (plist-get session :state)))
         (state-icon (when session
                       (task-conductor-project--task-execution-icon state))))
    (cond
     ((and state-icon (memq state '(:running :escalated)))
      (concat indent
              (propertize state-icon 'task-conductor-task-id task-id)
              " "
              (propertize "⏹"
                          'task-conductor-task-id task-id
                          'mouse-face 'highlight
                          'keymap task-conductor-project--stop-icon-map)
              " " base))
     (state-icon
      (concat indent (propertize state-icon 'task-conductor-task-id task-id) " " base))
     (session
      (message "task-conductor: unexpected session state %S for task %S" state task-id)
      (concat indent base))
     (t
      (concat indent
              (propertize "▶"
                          'task-conductor-task-id task-id
                          'mouse-face 'highlight
                          'keymap task-conductor-project--play-icon-map)
              " " base)))))

(defun task-conductor-project--insert-task-children (project-path)
  "Insert task child sections for PROJECT-PATH from cache only.
Does not call the CLI; inserts nothing on a cache miss.
On a cached error value, inserts a warning message."
  (when project-path
    (when-let ((tasks (gethash project-path task-conductor-project--task-cache)))
      (cond
       ((eq :error (car tasks))
        (insert (propertize (format "    %s\n" (cadr tasks))
                            'face 'font-lock-warning-face)))
       (tasks
        (dolist (task tasks)
          (magit-insert-section (task-conductor-project-task task)
            (magit-insert-heading
              (format "%s\n"
                      (task-conductor-project--format-task-entry
                       task project-path))))))))))

;;; Section traversal and expansion tracking

(defun task-conductor-project--walk-sections (section fn)
  "Walk SECTION and its descendants depth-first, calling FN on each."
  (when section
    (funcall fn section)
    (dolist (child (oref section children))
      (task-conductor-project--walk-sections child fn))))

(defun task-conductor-project--expanded-paths ()
  "Return list of project paths whose entry sections are currently expanded."
  (let (paths)
    (when (and (boundp 'magit-root-section) magit-root-section)
      (task-conductor-project--walk-sections
       magit-root-section
       (lambda (section)
         (when (and (eq (oref section type) 'task-conductor-project-entry)
                    (not (oref section hidden)))
           (push (plist-get (oref section value) :project/path) paths)))))
    (nreverse paths)))

(defun task-conductor-project--hide-all-entries ()
  "Hide all project entry sections in the current buffer."
  (when (and (boundp 'magit-root-section) magit-root-section)
    (task-conductor-project--walk-sections
     magit-root-section
     (lambda (section)
       (when (eq (oref section type) 'task-conductor-project-entry)
         (magit-section-hide section))))))

(defun task-conductor-project--reexpand-paths (paths)
  "Show project entry sections whose paths are in PATHS."
  (when (and paths (boundp 'magit-root-section) magit-root-section)
    (task-conductor-project--walk-sections
     magit-root-section
     (lambda (section)
       (when (and (eq (oref section type) 'task-conductor-project-entry)
                  (member (plist-get (oref section value) :project/path) paths))
         (magit-section-show section))))))

;;; Lazy loading

(defun task-conductor-project--check-lazy-load ()
  "Lazy-load tasks for the current project section if not yet cached.
Run as a buffer-local `post-command-hook'.  When a project entry
section is expanded but has no children and no cache entry, fetches
tasks from the CLI, caches them, re-renders the buffer, and restores
expansion state."
  (when-let ((section (magit-current-section)))
    (when (and (eq (oref section type) 'task-conductor-project-entry)
               (not (oref section hidden))
               (null (oref section children)))
      (let* ((project (oref section value))
             (path (plist-get project :project/path)))
        (when (and path
                   (not (gethash path task-conductor-project--task-cache)))
          (let ((expanded (task-conductor-project--expanded-paths)))
            (puthash path
                     (task-conductor-project--fetch-tasks path)
                     task-conductor-project--task-cache)
            (when task-conductor-dev-env--cached-projects
              (task-conductor-project--render
               task-conductor-dev-env--cached-projects)
              (task-conductor-project--reexpand-paths expanded))))))))

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
PROJECTS is a list of plists with :project/name and :project/path.
All project entry sections start collapsed; callers should use
`task-conductor-project--reexpand-paths' to restore expansion state."
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
                        (task-conductor-project--format-entry p name-width)))
              (task-conductor-project--insert-task-children
               (plist-get p :project/path))))
        (insert "  (none)\n")))
    (task-conductor-project--hide-all-entries)
    (if saved-section-ident
        (when-let ((s (magit-get-section saved-section-ident)))
          (goto-char (oref s start)))
      (goto-char (point-min)))))

;;; Auto-refresh

(defun task-conductor-project-rerender-if-live ()
  "Re-render projects buffer from cached data if it exists.
Called by the :notify-projects-changed handler.  Preserves
expansion state and keeps the task cache intact."
  (when-let ((buf (get-buffer task-conductor-project--buffer-name)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((expanded (task-conductor-project--expanded-paths)))
          (task-conductor-project--render
           task-conductor-dev-env--cached-projects)
          (task-conductor-project--reexpand-paths expanded))))))

;;; Interactive commands

(defun task-conductor-project--task-context-at-point ()
  "Return plist with :task-id and :project-dir from task section at point.
Walks up from a task-conductor-project-task section to its parent
task-conductor-project-entry section to find the project directory.
Returns nil if not on a task section or project-dir is unavailable."
  (when-let ((section (magit-current-section)))
    (when (eq (oref section type) 'task-conductor-project-task)
      (let* ((task (oref section value))
             (task-id (plist-get task :id))
             (parent (oref section parent))
             (project-dir
              (when (and parent
                         (eq (oref parent type) 'task-conductor-project-entry))
                (plist-get (oref parent value) :project/path))))
        (when (and task-id project-dir)
          (list :task-id task-id :project-dir project-dir))))))

(defun task-conductor-project-execute ()
  "Start automated execution of the task or story at point.
Extracts task-id and project-dir from the task section at point,
then calls the execute! mutation via nREPL.  Shows a confirmation
message on success.  Signals `user-error' when not on a task or
when not connected to task-conductor."
  (interactive)
  (let ((ctx (task-conductor-project--task-context-at-point)))
    (unless ctx
      (user-error "No task at point"))
    (let* ((task-id (plist-get ctx :task-id))
           (project-dir (plist-get ctx :project-dir))
           (result (task-conductor-project--eval-or-error
                    (format
                     "(task-conductor.emacs-dev-env.interface/execute-task-by-id %S %S %d)"
                     task-conductor-dev-env--dev-env-id project-dir task-id))))
      (if (and (listp result) (eq :error (plist-get result :status)))
          (message "Error starting execution of task %d: %s"
                   task-id
                   (or (plist-get result :message) "unknown"))
        (message "Started execution of task %d" task-id)))))

(defun task-conductor-project-cancel ()
  "Cancel execution for the task at point.
Extracts the session-id from the matching cached session and calls
`stop!' on the statechart engine via nREPL.
Signals `user-error' when not on a task section, no active session
exists, the session is not running or escalated, or nREPL is not
connected."
  (interactive)
  (let ((ctx (task-conductor-project--task-context-at-point)))
    (unless ctx
      (user-error "No task at point"))
    (let* ((task-id (plist-get ctx :task-id))
           (project-dir (plist-get ctx :project-dir))
           (session (task-conductor-project--find-session task-id project-dir)))
      (unless session
        (user-error "No active session for task %d" task-id))
      (let ((state (plist-get session :state)))
        (unless (memq state '(:running :escalated))
          (user-error "Task %d is not running or escalated" task-id))
        (let* ((session-id (plist-get session :session-id))
               (result (task-conductor-project--eval-or-error
                        (format
                         "(task-conductor.statechart-engine.interface/stop! %S)"
                         session-id))))
          (if (and (listp result) (eq :error (plist-get result :status)))
              (user-error "Cancel failed: %s"
                          (or (plist-get result :message) "unknown"))
            (message "Cancelled execution for task %d" task-id)))))))

(defun task-conductor-project-refresh ()
  "Refresh the project list from the JVM.
Clears the task cache so tasks are re-fetched from CLI on next expand.
Pre-fetches tasks for currently-expanded projects so they remain visible
after refresh.  Preserves which project sections were expanded."
  (interactive)
  (let* ((expanded (task-conductor-project--expanded-paths)))
    ;; Clear first so non-expanded paths don't retain stale data, then
    ;; re-populate expanded paths before --render (which reads from
    ;; cache only — cache miss means no children shown).
    (unless task-conductor-project--task-cache
      (setq task-conductor-project--task-cache (make-hash-table :test #'equal)))
    (clrhash task-conductor-project--task-cache)
    (dolist (path expanded)
      (puthash path (task-conductor-project--fetch-tasks path)
               task-conductor-project--task-cache))
    (if (not (task-conductor-dev-env--connected-p))
        (progn
          (task-conductor-project--render nil)
          (message "Not connected to task-conductor"))
      (task-conductor-dev-env-query-sessions)
      (let ((result (task-conductor-project--list)))
        (if (eq :ok (plist-get result :status))
            (let ((projects (append (plist-get result :projects) nil)))
              (setq task-conductor-dev-env--cached-projects projects)
              (task-conductor-project--render projects)
              (task-conductor-project--reexpand-paths expanded)
              (message "Projects refreshed"))
          (message "Error fetching projects: %s"
                   (or (plist-get result :message) "unknown")))))))

(defun task-conductor-project-create ()
  "Create a new project.
Prompts for directory path and optional name."
  (interactive)
  (let* ((path (expand-file-name (read-directory-name "Project path: ")))
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

(defvar-keymap task-conductor-project-mode-map
  :doc "Keymap for `task-conductor-project-mode'."
  :parent magit-section-mode-map
  "g"   #'task-conductor-project-refresh
  "c"   #'task-conductor-project-create
  "d"   #'task-conductor-project-delete
  "e"   #'task-conductor-project-execute
  "k"   #'task-conductor-project-cancel
  "r"   #'task-conductor-project-rename
  "RET" #'task-conductor-project-open-dired
  "t"   #'task-conductor-project-open-tasks
  "q"   #'task-conductor-project-quit)

(define-derived-mode task-conductor-project-mode magit-section-mode
  "TC Projects"
  "Major mode for managing task-conductor projects.

\\{task-conductor-project-mode-map}"
  (setq task-conductor-project--task-cache (make-hash-table :test #'equal))
  (add-hook 'post-command-hook
            #'task-conductor-project--check-lazy-load
            nil t))

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
