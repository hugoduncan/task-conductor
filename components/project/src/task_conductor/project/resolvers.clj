(ns task-conductor.project.resolvers
  "EQL resolvers and mutations for project operations.
   Uses the atom-based registry for project management.
   Registered on namespace load."
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.claude-cli.interface :as claude-cli]
   [task-conductor.mcp-tasks.interface :as mcp-tasks]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.registry :as registry]
   [task-conductor.project.execute :as execute]
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

;;; Execute Mutation

(defn- fetch-task
  "Fetch task data via EQL query.
   Returns task map with :task/type, :task/status, etc.
   On error, the result contains :task/error from the resolver."
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
  Handles both keyword (:story) and string (\"story\") values."
  [task]
  (let [t (:task/type task)]
    (or (= :story t) (= "story" t))))

(defn- task->execute-map
  "Convert EQL task map to execute format (unnamespaced keys)."
  [task]
  {:status (keyword (name (or (:task/status task) :open)))
   :meta (:task/meta task)
   :pr-num (:task/pr-num task)
   :code-reviewed (:task/code-reviewed task)})

(defn- derive-initial-state
  "Derive initial state based on task type."
  [task children]
  (let [execute-task (task->execute-map task)]
    (if (story? task)
      (execute/derive-story-state execute-task
                                  (mapv task->execute-map children))
      (execute/derive-task-state execute-task))))

(graph/defmutation execute!
  "Start automated execution of a task or story.
   Initializes statechart session and registers dev-env hooks.

   Calls mcp-tasks work-on to get the worktree path, then uses that
   as the project directory for skill invocations and escalation.

   Input:
     :task/project-dir - project directory (where .mcp-tasks.edn lives)
     :task/id          - task or story ID

   Returns:
     :execute/session-id    - statechart session UUID
     :execute/initial-state - derived initial state keyword
     :execute/error         - error map if failed"
  [{:task/keys [project-dir id]}]
  {::pco/output [:execute/session-id :execute/initial-state :execute/error]}
  (let [worktree-result (mcp-tasks/work-on
                         {:project-dir project-dir :task-id id})]
    (if (:error worktree-result)
      {:execute/error worktree-result}
      (let [worktree-path (:worktree-path worktree-result)
            task (fetch-task worktree-path id)]
        (if (:task/error task)
          {:execute/error (:task/error task)}
          (let [is-story (story? task)
                children (when is-story
                           (fetch-children worktree-path id))
                chart-id (if is-story :execute/story :execute/task)
                initial-data {:project-dir worktree-path
                              :task-id id
                              :task-type (if is-story :story :task)}
                session-id (sc/start! chart-id {:data initial-data})
                initial-state (derive-initial-state task children)
                selected (graph/query [:dev-env/selected])
                dev-env-id (:dev-env/id
                            (:dev-env/selected selected))]
            (when dev-env-id
              (graph/query
               [`(task-conductor.statechart-engine.resolvers/engine-register-dev-env-hook!
                  {:dev-env/id ~dev-env-id
                   :dev-env/hook-type :on-idle
                   :engine/session-id ~session-id
                   :engine/event :complete})]))
            {:execute/session-id session-id
             :execute/initial-state initial-state}))))))

;;; Skill Invocation

(def ^:private active-skill-threads
  "Atom tracking virtual threads spawned by invoke-skill!.
   Used by tests to synchronize cleanup."
  (atom #{}))

(defn await-skill-threads!
  "Wait for all active skill threads to complete.
   Returns when no skill threads remain active.
   Loops to handle cascading invocations where completing one skill
   may trigger another. Primarily used by tests for synchronization."
  []
  (loop []
    (let [threads @active-skill-threads]
      (when (seq threads)
        (doseq [^Thread thread threads]
          (.join thread))
        ;; Check again for newly spawned threads
        (recur)))))

(defn reset-skill-threads!
  "Reset the skill threads tracking atom.
   Should be called during test cleanup."
  []
  (reset! active-skill-threads #{}))

(defn- no-progress?
  "Check if skill made no progress.
   For most states: new-state == pre-skill-state
   For :has-tasks: state unchanged AND open children count unchanged"
  [pre-skill-state new-state pre-open-children new-open-children]
  (let [was-has-tasks (= :has-tasks pre-skill-state)
        is-has-tasks (= :has-tasks new-state)]
    (if (and was-has-tasks is-has-tasks)
      ;; For :has-tasks state, check open children count
      (= pre-open-children new-open-children)
      ;; For other states, check if derived state matches
      (= pre-skill-state new-state))))

(defn- on-skill-complete
  "Handle skill completion. Re-derives state and sends event to statechart.
   Called from virtual thread when claude-cli promise delivers.
   Guards against session being stopped during skill execution.
   Detects no-progress and escalates to dev-env with Claude session-id."
  [session-id result]
  (try
    (let [data (sc/get-data session-id)
          {:keys [project-dir task-id task-type
                  pre-skill-state pre-skill-open-children]} data]
      (if (:error result)
        ;; Skill failed - send error event
        (sc/send! session-id :error)
        ;; Skill succeeded - re-derive state and check for progress
        (let [task (fetch-task project-dir task-id)
              children (when (= :story task-type)
                         (fetch-children project-dir task-id))
              children-maps (mapv task->execute-map children)
              new-state (if (= :story task-type)
                          (execute/derive-story-state
                           (task->execute-map task)
                           children-maps)
                          (execute/derive-task-state
                           (task->execute-map task)))
              new-open-children (when (= :story task-type)
                                  (execute/count-open-children children-maps))]
          (if (no-progress? pre-skill-state new-state
                            pre-skill-open-children new-open-children)
            ;; No progress - store Claude session-id and escalate
            (do
              (sc/update-data! session-id
                               #(assoc % :last-claude-session-id (:session-id result)))
              (sc/send! session-id :no-progress))
            ;; Progress made - send new state as event
            (sc/send! session-id new-state)))))
    (catch clojure.lang.ExceptionInfo e
      ;; Session was stopped during skill execution - ignore
      (when-not (= :session-not-found (:error (ex-data e)))
        (throw e)))))

(defn- store-pre-skill-state!
  "Store current derived state and open children count before skill invocation.
   Used by on-skill-complete to detect no-progress.
   Derives state from task data rather than sc/current-state because entry
   actions run before the sessions atom is updated."
  [session-id]
  (let [data (sc/get-data session-id)
        {:keys [project-dir task-id task-type]} data
        task (fetch-task project-dir task-id)
        children (when (= :story task-type)
                   (fetch-children project-dir task-id))
        children-maps (mapv task->execute-map children)
        ;; Derive current state from task data (matches how on-skill-complete works)
        current-derived-state (if (= :story task-type)
                                (execute/derive-story-state
                                 (task->execute-map task)
                                 children-maps)
                                (execute/derive-task-state
                                 (task->execute-map task)))
        open-children-count (when (= :story task-type)
                              (execute/count-open-children children-maps))]
    (sc/update-data! session-id
                     (fn [d]
                       (cond-> (assoc d :pre-skill-state current-derived-state)
                         (some? open-children-count)
                         (assoc :pre-skill-open-children open-children-count))))))

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
  (let [_ (store-pre-skill-state! session-id)
        data (sc/get-data session-id)
        {:keys [project-dir]} data
        prompt (str "/" skill)
        handle (claude-cli/invoke {:prompt prompt :dir project-dir})
        ;; Use bound-fn to convey dynamic bindings to virtual thread
        ;; (needed for nullable testing infrastructure)
        completion-fn (bound-fn []
                        (try
                          (let [result @(:result-promise handle)]
                            (on-skill-complete session-id result))
                          (finally
                            (swap! active-skill-threads disj (Thread/currentThread)))))
        thread (Thread/startVirtualThread completion-fn)]
    (swap! active-skill-threads conj thread)
    {:invoke-skill/status :started}))

(graph/defmutation escalate-to-dev-env!
  "Escalate to dev-env for human intervention.
   Called from statechart :escalated state entry action.

   Input (injected by statechart engine):
     :engine/session-id - statechart session ID

   Notifies the selected dev-env of the failure.
   If a Claude session-id is available (from no-progress escalation),
   passes it to dev-env for conversation resumption."
  [{:engine/keys [session-id]}]
  {::pco/output [:escalate/status :escalate/error]}
  (let [data (sc/get-data session-id)
        {:keys [project-dir task-id last-claude-session-id]} data
        selected (graph/query [:dev-env/selected])
        dev-env-id (:dev-env/id (:dev-env/selected selected))]
    (if dev-env-id
      ;; Start an interactive session in the dev-env for human intervention
      (do
        (graph/query [`(task-conductor.dev-env.resolvers/dev-env-start-session!
                        {:dev-env/id ~dev-env-id
                         :dev-env/session-id ~session-id
                         :dev-env/opts ~(cond-> {:dir project-dir
                                                 :task-id task-id}
                                          last-claude-session-id
                                          (assoc :claude-session-id last-claude-session-id))})])
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
   execute!
   invoke-skill!
   escalate-to-dev-env!])

(defn register-resolvers!
  "Register all project resolvers and mutations with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
