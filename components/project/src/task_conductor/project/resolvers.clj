(ns task-conductor.project.resolvers
  "EQL resolvers and mutations for project operations.
   Uses the atom-based registry for project management.
   Registered on namespace load."
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.claude-cli.interface :as claude-cli]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.registry :as registry]
   [task-conductor.project.work-on :as work-on]
   [task-conductor.statechart-engine.interface :as sc]))

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

;;; Work-on Mutation

(defn- fetch-task
  "Fetch task data via EQL query.
   Returns task map with :task/type, :task/status, etc. or :task/error."
  [project-dir task-id]
  (graph/query {:task/id task-id :task/project-dir project-dir}
               [:task/type :task/status :task/meta :task/pr-num
                :task/code-reviewed :task/error]))

(defn- fetch-children
  "Fetch children for a story via EQL query.
   Returns vector of child task maps."
  [project-dir parent-id]
  (let [result (graph/query {:task/project-dir project-dir
                             :task/filters {:parent-id parent-id}}
                            [:task/all])]
    (:task/all result)))

(defn- story?
  "Check if task is a story type.
  Assumes :task/type is a keyword from EDN parsing (e.g., :story, :task)."
  [task]
  (= :story (:task/type task)))

(defn- task->work-on-map
  "Convert EQL task map to work-on format (unnamespaced keys)."
  [task]
  {:status (keyword (name (or (:task/status task) :open)))
   :meta (:task/meta task)
   :pr-num (:task/pr-num task)
   :code-reviewed (:task/code-reviewed task)})

(defn- derive-initial-state
  "Derive initial state based on task type."
  [task children]
  (let [work-on-task (task->work-on-map task)]
    (if (story? task)
      (work-on/derive-story-state work-on-task
                                  (mapv task->work-on-map children))
      (work-on/derive-task-state work-on-task))))

(graph/defmutation work-on!
  "Start automated execution of a task or story.
   Initializes statechart session and registers dev-env hooks.

   Input:
     :task/project-dir - project directory
     :task/id          - task or story ID

   Returns:
     :work-on/session-id    - statechart session UUID
     :work-on/initial-state - derived initial state keyword
     :work-on/error         - error map if failed"
  [{:task/keys [project-dir id]}]
  {::pco/output [:work-on/session-id :work-on/initial-state :work-on/error]}
  (let [task (fetch-task project-dir id)]
    (if (:task/error task)
      {:work-on/error (:task/error task)}
      (let [is-story (story? task)
            children (when is-story (fetch-children project-dir id))
            chart-id (if is-story :work-on/story :work-on/task)
            initial-data {:project-dir project-dir
                          :task-id id
                          :task-type (if is-story :story :task)}
            session-id (sc/start! chart-id {:data initial-data})
            initial-state (derive-initial-state task children)
            ;; Register dev-env hook for PR merge notification
            selected (graph/query [:dev-env/selected])
            dev-env-id (:dev-env/id (:dev-env/selected selected))]
        (when dev-env-id
          (graph/query [`(task-conductor.statechart-engine.resolvers/engine-register-dev-env-hook!
                          {:dev-env/id ~dev-env-id
                           :dev-env/hook-type :on-idle
                           :engine/session-id ~session-id
                           :engine/event :complete})]))
        {:work-on/session-id session-id
         :work-on/initial-state initial-state}))))

;;; Skill Invocation

(defn- on-skill-complete
  "Handle skill completion. Re-derives state and sends event to statechart.
   Called from virtual thread when claude-cli promise delivers.
   Guards against session being stopped during skill execution."
  [session-id result]
  (try
    (let [data (sc/get-data session-id)
          {:keys [project-dir task-id task-type]} data]
      (if (:error result)
        ;; Skill failed - send error event
        (sc/send! session-id :error)
        ;; Skill succeeded - re-derive state and send as event
        (let [task (fetch-task project-dir task-id)
              children (when (= :story task-type)
                         (fetch-children project-dir task-id))
              new-state (if (= :story task-type)
                          (work-on/derive-story-state
                           (task->work-on-map task)
                           (mapv task->work-on-map children))
                          (work-on/derive-task-state
                           (task->work-on-map task)))]
          (sc/send! session-id new-state))))
    (catch clojure.lang.ExceptionInfo e
      ;; Session was stopped during skill execution - ignore
      (when-not (= :session-not-found (:error (ex-data e)))
        (throw e)))))

(graph/defmutation invoke-skill!
  "Invoke a skill via claude-cli based on current statechart state.
   Called from statechart entry actions. Spawns virtual thread to
   wait for completion and trigger next transition.

   Input (injected by statechart engine):
     :engine/session-id - statechart session ID
     :skill             - skill name to invoke

   Returns immediately with invocation status."
  [{:keys [skill] :engine/keys [session-id]}]
  {::pco/output [:invoke-skill/status :invoke-skill/error]}
  (let [data (sc/get-data session-id)
        {:keys [project-dir]} data
        prompt (str "/" skill)
        handle (claude-cli/invoke {:prompt prompt :dir project-dir})]
    ;; Spawn virtual thread to wait for completion
    (Thread/startVirtualThread
     (fn []
       (let [result @(:result-promise handle)]
         (on-skill-complete session-id result))))
    {:invoke-skill/status :started}))

(graph/defmutation escalate-to-dev-env!
  "Escalate to dev-env for human intervention.
   Called from statechart :escalated state entry action.

   Input (injected by statechart engine):
     :engine/session-id - statechart session ID

   Notifies the selected dev-env of the failure."
  [{:engine/keys [session-id]}]
  {::pco/output [:escalate/status :escalate/error]}
  (let [data (sc/get-data session-id)
        {:keys [project-dir task-id]} data
        selected (graph/query [:dev-env/selected])
        dev-env-id (:dev-env/id (:dev-env/selected selected))]
    (if dev-env-id
      ;; Start an interactive session in the dev-env for human intervention
      (do
        (graph/query [`(task-conductor.dev-env.resolvers/dev-env-start-session!
                        {:dev-env/id ~dev-env-id
                         :dev-env/session-id ~session-id
                         :dev-env/opts {:dir ~project-dir
                                        :task-id ~task-id}})])
        {:escalate/status :escalated
         :escalate/dev-env-id dev-env-id})
      {:escalate/status :no-dev-env
       :escalate/error {:error :no-dev-env
                        :message "No dev-env available for escalation"}})))

;;; Registration

(def all-operations
  "Vector of all project resolvers and mutations."
  [project-by-path
   project-by-name
   all-projects
   project-create!
   project-update!
   project-delete!
   work-on!
   invoke-skill!
   escalate-to-dev-env!])

(defn register-resolvers!
  "Register all project resolvers and mutations with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
