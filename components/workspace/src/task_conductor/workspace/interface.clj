(ns task-conductor.workspace.interface
  "Workspace management for multi-project coordination.

   Provides functions to manage workspaces containing multiple projects,
   track focused project, and query project tasks and git worktrees."
  (:require
   [task-conductor.workspace.core :as core]))

(defn set-focused-project!
  "Set the currently focused project directory."
  [dir]
  (core/set-focused-project! dir))

(defn focused-project
  "Get the currently focused project directory."
  []
  (core/focused-project))

(defn add-project!
  "Add a project directory to the workspace."
  [dir]
  (core/add-project! dir))

(defn remove-project!
  "Remove a project directory from the workspace."
  [dir]
  (core/remove-project! dir))

(defn list-projects
  "List all project directories in the workspace."
  []
  (core/list-projects))

(defn project-config
  "Read .task-conductor.edn from project directory.

   Returns {:ok config-map} if file exists and parses successfully,
   {:ok nil} if file does not exist, or {:error message} on parse errors."
  [dir]
  (core/project-config dir))

(defn project-worktrees
  "Get git worktrees for a project.

   Uses :main-checkout from .task-conductor.edn config if present,
   otherwise uses the project directory itself.

   Returns {:ok [{:path \"...\" :branch \"...\" :commit \"...\" :bare? bool} ...]}
   or {:error message} on failure."
  [dir]
  (core/project-worktrees dir))

(defn project-tasks
  "Get tasks for a project via mcp-tasks CLI.

   Runs mcp-tasks list --format edn from the project directory.

   Returns {:ok [tasks...]} or {:error message} on failure."
  [dir]
  (core/project-tasks dir))
