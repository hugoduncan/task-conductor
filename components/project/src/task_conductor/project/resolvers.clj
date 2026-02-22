(ns task-conductor.project.resolvers
  "EQL resolvers and mutations for project operations.
   Uses the atom-based registry for project management.
   Registered on namespace load."
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [clojure.string :as str]
   [task-conductor.claude-cli.interface :as claude-cli]
   [task-conductor.dev-env.protocol :as dev-env]
   [task-conductor.dev-env.registry :as dev-env-registry]
   [task-conductor.mcp-tasks.interface :as mcp-tasks]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.registry :as registry]
   [task-conductor.project.execute :as execute]
   [task-conductor.statechart-engine.interface :as sc]
   [babashka.fs :as fs]
   [babashka.process :as process]
   [taoensso.timbre :as log]))

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

(defn- pr-merged?
  "Check if a PR has been merged on GitHub.
   Shells out to `gh pr view` in the given project directory.
   Returns true when merged, false otherwise (including on error)."
  [project-dir pr-num]
  (try
    (let [result (process/shell
                  {:dir project-dir :out :string :err :string}
                  "gh" "pr" "view" (str pr-num)
                  "--json" "state" "--jq" ".state")]
      (= "MERGED" (str/trim (:out result))))
    (catch Exception _
      false)))

(defn- task->execute-map
  "Convert EQL task map to execute format (unnamespaced keys).
   When project-dir is provided and task has a :pr-num, checks
   GitHub for merge status."
  ([task] (task->execute-map task nil))
  ([task project-dir]
   (let [pr-num (:task/pr-num task)]
     {:status (keyword (name (or (:task/status task) :open)))
      :meta (:task/meta task)
      :pr-num pr-num
      :code-reviewed (:task/code-reviewed task)
      :pr-merged? (when (and pr-num project-dir)
                    (pr-merged? project-dir pr-num))})))

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
     :task/project-dir  - project directory (where .mcp-tasks.edn lives)
     :task/id           - task or story ID
     :task/nrepl-port   - optional nREPL port for session hooks

   Returns:
     :execute/session-id    - statechart session UUID
     :execute/initial-state - derived initial state keyword
     :execute/error         - error map if failed"
  [{:task/keys [project-dir id nrepl-port]}]
  {::pco/output [:execute/session-id :execute/initial-state :execute/error]}
  (let [worktree-result (mcp-tasks/work-on
                         {:project-dir project-dir :task-id id})]
    (if (:error worktree-result)
      {:execute/session-id nil
       :execute/initial-state nil
       :execute/error worktree-result}
      (let [worktree-path (:worktree-path worktree-result)
            task (fetch-task worktree-path id)]
        (if (:task/error task)
          {:execute/session-id nil
           :execute/initial-state nil
           :execute/error (:task/error task)}
          (let [is-story (story? task)
                children (when is-story
                           (fetch-children worktree-path id))
                chart-id (if is-story :execute/story :execute/task)
                initial-data (cond-> {:project-dir worktree-path
                                      :root-project-dir project-dir
                                      :task-id id
                                      :task-type (if is-story :story :task)}
                               nrepl-port (assoc :nrepl-port nrepl-port))
                session-id (sc/start! chart-id {:data initial-data})
                initial-state (derive-initial-state task children)]
            {:execute/session-id session-id
             :execute/initial-state initial-state
             :execute/error nil}))))))

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
   For :has-tasks: state unchanged AND completed children count unchanged.
   Tracks completed count (monotonically increasing) rather than open count,
   because new tasks can be created during execution, masking progress.
   :complete is never no-progress — it means the story is closed."
  [pre-skill-state new-state pre-completed-children new-completed-children]
  (if (= :complete new-state)
    false
    (let [was-has-tasks (= :has-tasks pre-skill-state)
          is-has-tasks (= :has-tasks new-state)]
      (if (and was-has-tasks is-has-tasks)
        (= pre-completed-children new-completed-children)
        (= pre-skill-state new-state)))))

(defn- skill-failed?
  "Return true when the CLI result indicates the skill did not succeed.
   Checks both explicit :error key (timeout/interrupt) and non-zero exit code."
  [result]
  (or (:error result)
      (and (:exit-code result) (not (zero? (:exit-code result))))))

(defn- on-skill-complete
  "Handle skill completion. Re-derives state and sends event to statechart.
   Called from virtual thread when claude-cli promise delivers.
   Guards against session being stopped during skill execution.
   Detects no-progress and escalates to dev-env with Claude session-id.
   When :on-complete is set, sends it directly on success — skips
   re-derivation to avoid races with external state propagation."
  [session-id result]
  (try
    (let [data (sc/get-data session-id)
          {:keys [project-dir task-id task-type
                  pre-skill-state pre-skill-completed-children
                  on-complete]} data]
      (cond
        (skill-failed? result)
        (sc/send! session-id :error)

        ;; When on-complete is set, trust the skill result and skip
        ;; re-derivation. Avoids races where external state (e.g. GitHub
        ;; API) hasn't propagated yet.
        on-complete
        (sc/send! session-id on-complete)

        :else
        (let [task (fetch-task project-dir task-id)
              children (when (= :story task-type)
                         (fetch-children project-dir task-id))
              children-maps (mapv #(task->execute-map % project-dir)
                                  children)
              new-state (if (= :story task-type)
                          (execute/derive-story-state
                           (task->execute-map task project-dir)
                           children-maps)
                          (execute/derive-task-state
                           (task->execute-map task project-dir)))
              new-completed-children (when (= :story task-type)
                                       (execute/count-completed-children
                                        children-maps))]
          (if (no-progress? pre-skill-state new-state
                            pre-skill-completed-children
                            new-completed-children)
            (do
              (sc/update-data! session-id
                               #(assoc
                                 %
                                 :last-claude-session-id
                                 (:session-id result)))
              (sc/send! session-id :no-progress))
            (sc/send! session-id new-state)))))
    (catch clojure.lang.ExceptionInfo e
      ;; Session was stopped during skill execution - ignore
      (when-not (= :session-not-found (:error (ex-data e)))
        (throw e)))))

(defn- read-nrepl-port
  "Read the nREPL port from .nrepl-port in the given directory."
  [dir]
  (try
    (let [port-file (fs/path dir ".nrepl-port")]
      (if (fs/exists? port-file)
        (let [port (str/trim (slurp (fs/file port-file)))]
          (log/debug "nREPL port resolved" {:dir dir :port port})
          port)
        (do
          (log/debug "nREPL port file not found" {:dir dir})
          nil)))
    (catch java.io.IOException e
      (log/warn "Failed to read nREPL port file"
                {:dir dir :error (.getMessage e)})
      nil)))

(defn- build-session-hooks
  "Build CLI hooks map for idle/active detection during escalated sessions.
  Uses clj-nrepl-eval to send statechart events:
    UserPromptSubmit -> :on-active (Claude started working)
    Notification     -> :on-session-idle (Claude finished, waiting)"
  [session-id nrepl-port]
  (when nrepl-port
    (let [ns-fn "task-conductor.statechart-engine.interface"
          ;; Heredoc quoting avoids shell mangling of ! in send!
          send-event (fn [event-type]
                       (str "read -r -d '' CODE << 'EoC' || true\n"
                            "(" ns-fn "/send! \"" session-id "\" "
                            event-type ")\n"
                            "EoC\n"
                            "clj-nrepl-eval -p " nrepl-port
                            " \"$CODE\""))
          active (send-event ":on-active")
          idle (send-event ":on-session-idle")]
      (log/info "Built session hooks for idle/active detection"
                {:session-id session-id :nrepl-port nrepl-port})
      (log/debug "Session hook commands"
                 {:session-id session-id
                  :on-active-cmd active
                  :on-session-idle-cmd idle})
      {:UserPromptSubmit
       [{:hooks [{:type "command" :command active}]}]
       :Notification
       [{:hooks [{:type "command" :command idle}]}]})))

(defn- on-dev-env-close
  "Handle dev-env session close.
   Re-derives state and sends event to statechart.
   Called when human closes interactive dev-env buffer.
   Guards against session stopped while dev-env was open."
  [session-id _context]
  (try
    (let [data (sc/get-data session-id)
          {:keys [project-dir task-id task-type]} data
          task (fetch-task project-dir task-id)
          children (when (= :story task-type)
                     (fetch-children project-dir task-id))
          new-state (if (= :story task-type)
                      (execute/derive-story-state
                       (task->execute-map task)
                       (mapv task->execute-map children))
                      (execute/derive-task-state
                       (task->execute-map task)))]
      (sc/send! session-id new-state))
    (catch clojure.lang.ExceptionInfo e
      (when-not (= :session-not-found (:error (ex-data e)))
        (throw e)))))

(defn- store-pre-skill-state!
  "Store derived state and completed children count before skill invocation.
   Used by on-skill-complete to detect no-progress.
   Derives state from task data rather than sc/current-state because entry
   actions run before the sessions atom is updated."
  [session-id]
  (let [data (sc/get-data session-id)
        {:keys [project-dir task-id task-type]} data
        task (fetch-task project-dir task-id)
        children (when (= :story task-type)
                   (fetch-children project-dir task-id))
        children-maps (mapv #(task->execute-map % project-dir)
                            children)
        ;; Derive current state from task data
        ;; (matches how on-skill-complete works)
        current-derived-state (if (= :story task-type)
                                (execute/derive-story-state
                                 (task->execute-map task project-dir)
                                 children-maps)
                                (execute/derive-task-state
                                 (task->execute-map task project-dir)))
        completed-children-count
        (when (= :story task-type)
          (execute/count-completed-children children-maps))]
    (sc/update-data! session-id
                     (fn [d]
                       (cond-> (assoc d :pre-skill-state current-derived-state)
                         (some? completed-children-count)
                         (assoc
                          :pre-skill-completed-children
                          completed-children-count))))))

(graph/defmutation invoke-skill!
  "Invoke a skill via claude-cli based on current statechart state.
   Called from statechart entry actions. Spawns virtual thread to
   wait for completion and trigger next transition.

   Input (injected by statechart engine):
     :engine/session-id - statechart session ID
     :skill             - skill name to invoke
     :on-complete       - optional event keyword to send on success
                          instead of re-deriving state

   Returns immediately with invocation status."
  [{:keys [skill args on-complete] :engine/keys [session-id]}]
  {::pco/output [:invoke-skill/status :invoke-skill/error]}
  (let [_ (sc/update-data! session-id #(assoc % :on-complete on-complete))
        _ (store-pre-skill-state! session-id)
        data (sc/get-data session-id)
        {:keys [project-dir task-id]} data
        resolved-args (when args
                        (str/replace
                         args "{task-id}" (str task-id)))
        prompt (cond-> (str "/" skill)
                 resolved-args (str " " resolved-args))
        handle (claude-cli/invoke {:prompt prompt :dir project-dir})
        ;; Use bound-fn to convey dynamic bindings to virtual thread
        ;; (needed for nullable testing infrastructure)
        completion-fn (bound-fn []
                        (try
                          (let [result @(:result-promise handle)]
                            (on-skill-complete session-id result))
                          (finally
                            (swap!
                             active-skill-threads
                             disj
                             (Thread/currentThread)))))
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
  {::pco/output [:escalate/status :escalate/error :escalate/dev-env-id]}
  (let [data (sc/get-data session-id)
        {:keys [project-dir root-project-dir task-id
                last-claude-session-id nrepl-port]} data
        selected (graph/query [:dev-env/selected])
        dev-env-id (:dev-env/id (:dev-env/selected selected))]
    (if dev-env-id
      ;; Start an interactive session in the dev-env for human intervention
      (let [dev-env-instance (dev-env-registry/get-dev-env dev-env-id)
            ;; Prefer port from session data (set at bootstrap);
            ;; fall back to .nrepl-port file in worktree, then in original
            ;; project root (worktree may lack the file if it differs from root)
            nrepl-port (or nrepl-port
                           (when project-dir
                             (read-nrepl-port project-dir))
                           (when root-project-dir
                             (read-nrepl-port root-project-dir)))
            _ (if nrepl-port
                (log/info "nREPL port resolved for escalation"
                          {:session-id session-id
                           :nrepl-port nrepl-port
                           :project-dir project-dir
                           :root-project-dir root-project-dir})
                (log/warn
                 "No nREPL port available; idle/active detection disabled"
                 {:project-dir project-dir
                  :root-project-dir root-project-dir
                  :session-id session-id}))
            cli-hooks (build-session-hooks session-id nrepl-port)
            _ (log/info "Escalating session to dev-env"
                        {:session-id session-id
                         :dev-env-id dev-env-id
                         :has-hooks (boolean cli-hooks)
                         :has-claude-session (boolean last-claude-session-id)})
            opts (cond-> {:dir project-dir
                          :task-id task-id}
                   last-claude-session-id
                   (assoc :claude-session-id last-claude-session-id)
                   cli-hooks
                   (assoc :hooks cli-hooks))]
        (graph/query [`(task-conductor.dev-env.resolvers/dev-env-start-session!
                        {:dev-env/id ~dev-env-id
                         :dev-env/session-id ~session-id
                         :dev-env/opts ~opts})])
        ;; Register on-close hook to resume workflow when human finishes
        (when dev-env-instance
          (dev-env/register-hook dev-env-instance session-id :on-close
                                 (partial on-dev-env-close session-id)))
        {:escalate/status :escalated
         :escalate/dev-env-id dev-env-id})
      {:escalate/status :no-dev-env
       :escalate/error {:error :no-dev-env
                        :message "No dev-env available for escalation"}})))

;;; Manual Escalation

(graph/defmutation manual-escalate!
  "Open a dev-env session for manual intervention without changing
  statechart state. On session close, re-derives state and sends
  the derived event to the statechart.

  Input:
    :engine/session-id - statechart session ID

  Returns:
    :manual-escalate/status   - :escalated or :error
    :manual-escalate/error    - error map if failed"
  [{:engine/keys [session-id]}]
  {::pco/output [:manual-escalate/status :manual-escalate/error]}
  (try
    (let [data (sc/get-data session-id)
          {:keys [project-dir task-id last-claude-session-id]} data
          selected (graph/query [:dev-env/selected])
          dev-env-id (:dev-env/id (:dev-env/selected selected))]
      (if dev-env-id
        (let [dev-env-instance (dev-env-registry/get-dev-env dev-env-id)
              opts (cond-> {:dir project-dir
                            :task-id task-id}
                     last-claude-session-id
                     (assoc :claude-session-id last-claude-session-id))]
          (graph/query
           [`(task-conductor.dev-env.resolvers/dev-env-start-session!
              {:dev-env/id ~dev-env-id
               :dev-env/session-id ~session-id
               :dev-env/opts ~opts})])
          (when dev-env-instance
            (dev-env/register-hook dev-env-instance session-id :on-close
                                   (partial on-dev-env-close session-id)))
          {:manual-escalate/status :escalated
           :manual-escalate/error nil})
        {:manual-escalate/status :error
         :manual-escalate/error {:error :no-dev-env
                                 :message "No dev-env available"}}))
    (catch clojure.lang.ExceptionInfo e
      (if (= :session-not-found (:error (ex-data e)))
        {:manual-escalate/status :error
         :manual-escalate/error {:error :session-not-found
                                 :message "Session not found"}}
        (throw e)))))

;;; PR Merge Mutation

(graph/defmutation pr-merge!
  "Trigger PR merge for a session in :wait-pr-merge state.
   Sends :merge-pr event to the statechart, which transitions to
   :merging-pr and runs the squash-merge-on-gh skill.

   Input:
     :engine/session-id - statechart session ID

   Returns:
     :pr-merge/status - :triggered or :error
     :pr-merge/error  - error map if failed"
  [{:engine/keys [session-id]}]
  {::pco/output [:pr-merge/status :pr-merge/error]}
  (try
    (let [state (sc/current-state session-id)]
      (if (contains? state :wait-pr-merge)
        (do
          (sc/send! session-id :merge-pr)
          ;; nil :pr-merge/error required: Pathom expects all ::pco/output
          ;; keys present even on success (see LEARNING.md).
          {:pr-merge/status :triggered
           :pr-merge/error nil})
        {:pr-merge/status :error
         :pr-merge/error {:error :invalid-state
                          :message (str "Session not in :wait-pr-merge state, "
                                        "current: " state)}}))
    (catch clojure.lang.ExceptionInfo e
      (if (= :session-not-found (:error (ex-data e)))
        {:pr-merge/status :error
         :pr-merge/error {:error :session-not-found
                          :message "Session not found"}}
        (throw e)))))

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
   escalate-to-dev-env!
   manual-escalate!
   pr-merge!])

(defn register-resolvers!
  "Register all project resolvers and mutations.
   Called on namespace load. Can be called after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
