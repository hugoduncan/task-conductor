(ns task-conductor.project.resolvers
  "EQL resolvers and mutations for project operations.
   Uses the atom-based registry for project management.
   Registered on namespace load."
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.registry :as registry]))

;;; Resolvers

(graph/defresolver project-by-path
  "Lookup project by canonical path."
  [{:project/keys [path]}]
  {::pco/input [:project/path]
   ::pco/output [:project/name]}
  (if-let [project (registry/get-by-path path)]
    (select-keys project [:project/name])
    {:project/name nil}))

(graph/defresolver project-by-name
  "Lookup project by name."
  [{:project/keys [name]}]
  {::pco/input [:project/name]
   ::pco/output [:project/path]}
  (if-let [project (registry/get-by-name name)]
    (select-keys project [:project/path])
    {:project/path nil}))

(graph/defresolver all-projects
  "List all registered projects."
  []
  {::pco/output [:project/all]}
  {:project/all (registry/list-all)})

;;; Mutations

(graph/defmutation project-create!
  "Create a new project. Returns project map or error map."
  [{:project/keys [path name]}]
  {::pco/output [:project/result]}
  {:project/result
   (if name
     (registry/register! path {:project/name name})
     (registry/register! path))})

(graph/defmutation project-update!
  "Update project name. Returns updated project or error map."
  [{:project/keys [path name]}]
  {::pco/output [:project/result]}
  {:project/result (registry/update! path {:project/name name})})

(graph/defmutation project-delete!
  "Delete project by path. Returns deleted project or nil."
  [{:project/keys [path]}]
  {::pco/output [:project/result]}
  {:project/result (registry/unregister! path)})

;;; Registration

(def all-operations
  "Vector of all project resolvers and mutations."
  [project-by-path
   project-by-name
   all-projects
   project-create!
   project-update!
   project-delete!])

(defn register-resolvers!
  "Register all project resolvers and mutations with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
