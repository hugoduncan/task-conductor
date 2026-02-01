(ns task-conductor.mcp-tasks.resolvers
  "EQL resolvers and mutations for mcp-tasks operations.
   Wraps CLI interface functions with :task/ namespaced attributes.
   Registered on namespace load."
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.mcp-tasks.interface :as interface]
   [task-conductor.pathom-graph.interface :as graph]))

;;; Helpers

(defn- prefix-keys
  "Add :task/ namespace prefix to all keys in a map."
  [m]
  (when m
    (into {}
          (map (fn [[k v]]
                 [(keyword "task" (name k)) v]))
          m)))

(defn- unprefix-keys
  "Remove :task/ namespace prefix from keys, keeping only task-namespaced keys."
  [m]
  (into {}
        (keep (fn [[k v]]
                (when (= "task" (namespace k))
                  [(keyword (name k)) v])))
        m))

;;; Resolvers

(graph/defresolver task-by-id
  "Lookup task by ID. Requires :task/id and :task/project-dir."
  [{:task/keys [id project-dir]}]
  {::pco/input [:task/id :task/project-dir]
   ::pco/output [:task/title :task/description :task/status :task/category
                 :task/type :task/parent-id :task/relations :task/meta
                 :task/design :task/shared-context :task/blocking-task-ids
                 :task/is-blocked :task/pr-num :task/code-reviewed
                 (pco/? :task/error)]}
  (let [result (interface/show-task {:project-dir project-dir :task-id id})]
    (if (:error result)
      {:task/error result}
      (prefix-keys (:task result)))))

(graph/defresolver tasks-list
  "List tasks with optional filters. Requires :task/project-dir.
   Filter params can be passed via :task/filters map containing:
   :status, :category, :type, :parent-id, :blocked, :limit, :title-pattern"
  [{:task/keys [project-dir filters]}]
  {::pco/input [:task/project-dir (pco/? :task/filters)]
   ::pco/output [:task/all :task/metadata (pco/? :task/error)]}
  (let [opts (merge {:project-dir project-dir} filters)
        result (interface/list-tasks opts)]
    (if (:error result)
      {:task/error result}
      {:task/all (mapv prefix-keys (:tasks result))
       :task/metadata (:metadata result)})))

(graph/defresolver task-blocking-info
  "Get blocking information for a task. Requires :task/id and :task/project-dir."
  [{:task/keys [id project-dir]}]
  {::pco/input [:task/id :task/project-dir]
   ::pco/output [:task/blocked-by :task/blocking-reason (pco/? :task/error)]}
  (let [result (interface/why-blocked {:project-dir project-dir :task-id id})]
    (if (:error result)
      {:task/error result}
      (prefix-keys result))))

;;; Mutations

(graph/defmutation task-create!
  "Create a new task. Returns :task/result with created task or error."
  [params]
  {::pco/output [:task/result]}
  (let [{:task/keys [project-dir category title description type parent-id prepend]} params
        opts (cond-> {:project-dir project-dir
                      :category category
                      :title title}
               description (assoc :description description)
               type (assoc :type type)
               parent-id (assoc :parent-id parent-id)
               prepend (assoc :prepend prepend))]
    {:task/result (interface/add-task opts)}))

(graph/defmutation task-complete!
  "Complete a task. Returns :task/result with updated task or error."
  [params]
  {::pco/output [:task/result]}
  (let [{:task/keys [project-dir id title category comment]} params
        opts (cond-> {:project-dir project-dir}
               id (assoc :task-id id)
               title (assoc :title title)
               category (assoc :category category)
               comment (assoc :comment comment))]
    {:task/result (interface/complete-task opts)}))

(graph/defmutation task-update!
  "Update task fields. Returns :task/result with updated task or error."
  [params]
  {::pco/output [:task/result]}
  (let [{:task/keys [project-dir id]} params
        ;; Extract update fields from params
        update-keys [:title :description :design :status :category :type
                     :parent-id :meta :relations :session-events
                     :shared-context :code-reviewed :pr-num]
        updates (select-keys (unprefix-keys params) update-keys)
        opts (merge {:project-dir project-dir :task-id id} updates)]
    {:task/result (interface/update-task opts)}))

(graph/defmutation task-delete!
  "Delete a task. Returns :task/result with deletion result or error."
  [params]
  {::pco/output [:task/result]}
  (let [{:task/keys [project-dir id title-pattern]} params
        opts (cond-> {:project-dir project-dir}
               id (assoc :task-id id)
               title-pattern (assoc :title-pattern title-pattern))]
    {:task/result (interface/delete-task opts)}))

(graph/defmutation task-reopen!
  "Reopen a closed task. Returns :task/result with reopened task or error."
  [params]
  {::pco/output [:task/result]}
  (let [{:task/keys [project-dir id title-pattern]} params
        opts (cond-> {:project-dir project-dir}
               id (assoc :task-id id)
               title-pattern (assoc :title-pattern title-pattern))]
    {:task/result (interface/reopen-task opts)}))

;;; Registration

(def all-operations
  "Vector of all mcp-tasks resolvers and mutations."
  [task-by-id
   tasks-list
   task-blocking-info
   task-create!
   task-complete!
   task-update!
   task-delete!
   task-reopen!])

(defn register-resolvers!
  "Register all mcp-tasks resolvers and mutations with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
