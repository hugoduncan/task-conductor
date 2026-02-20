;;; task-conductor-project.el --- Project management for task-conductor -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: task-conductor contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, processes
;; URL: https://github.com/hugoduncan/task-conductor

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; nREPL convenience functions for project CRUD operations via the
;; JVM project registry.  These functions provide the communication
;; layer between Emacs and the Clojure project component.
;;
;; All functions require an active dev-env connection
;; (see `task-conductor-dev-env-connect').

;;; Code:

(require 'task-conductor-dev-env)

;;; Project CRUD

(defun task-conductor-project--list ()
  "Query all registered projects from the JVM.
Returns a plist with :status and :projects on success."
  (unless (task-conductor-dev-env--connected-p)
    (user-error "Not connected to task-conductor"))
  (task-conductor-dev-env--eval-sync
   (format "(task-conductor.emacs-dev-env.interface/query-projects-by-id %S)"
           task-conductor-dev-env--dev-env-id)))

(defun task-conductor-project--create (path &optional name)
  "Create a project at PATH with optional NAME.
Returns a plist with :status and :project on success."
  (unless (task-conductor-dev-env--connected-p)
    (user-error "Not connected to task-conductor"))
  (task-conductor-dev-env--eval-sync
   (format "(task-conductor.emacs-dev-env.interface/create-project-by-id %S %S %S)"
           task-conductor-dev-env--dev-env-id
           path
           name)))

(defun task-conductor-project--update (path name)
  "Update project at PATH to have NAME.
Returns a plist with :status and :project on success."
  (unless (task-conductor-dev-env--connected-p)
    (user-error "Not connected to task-conductor"))
  (task-conductor-dev-env--eval-sync
   (format "(task-conductor.emacs-dev-env.interface/update-project-by-id %S %S %S)"
           task-conductor-dev-env--dev-env-id
           path
           name)))

(defun task-conductor-project--delete (path)
  "Delete project at PATH.
Returns a plist with :status and :project on success."
  (unless (task-conductor-dev-env--connected-p)
    (user-error "Not connected to task-conductor"))
  (task-conductor-dev-env--eval-sync
   (format "(task-conductor.emacs-dev-env.interface/delete-project-by-id %S %S)"
           task-conductor-dev-env--dev-env-id
           path)))

(provide 'task-conductor-project)
;;; task-conductor-project.el ends here
