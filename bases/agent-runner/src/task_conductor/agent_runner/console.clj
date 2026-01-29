(ns task-conductor.agent-runner.console
  "Console state machine for orchestrating story execution.

   Manages workflow state transitions between SDK agent execution,
   CLI handoff, and task completion. This namespace provides pure
   transition functions and mutable wrappers with history tracking.

   State is scoped by workspace path. Each workspace has independent
   state allowing concurrent story execution across projects."
  (:require
   [babashka.process :as p]
   [clojure.string :as str]
   [task-conductor.agent-runner.handoff :as handoff]
   [task-conductor.dev-env.interface :as dev-env]
   [task-conductor.workspace.interface :as workspace]))

;;; State Machine Definition

(def valid-transitions
  "Map of valid state transitions.
   Each key is a source state, value is set of valid target states."
  {:idle            #{:selecting-task}
   :selecting-task  #{:running-sdk :story-complete :error-recovery}
   :running-sdk     #{:needs-input :task-complete :error-recovery}
   :needs-input     #{:running-cli :error-recovery}
   :running-cli     #{:running-sdk :error-recovery}
   :error-recovery  #{:selecting-task :running-sdk :idle}
   :task-complete   #{:selecting-task :story-complete :error-recovery}
   :story-complete  #{:idle}})

(def all-states
  "Set of all valid states in the state machine.
   States: :idle, :selecting-task, :running-sdk, :needs-input,
           :running-cli, :error-recovery, :task-complete, :story-complete."
  (into #{} (keys valid-transitions)))

;;; Handoff Integration

(def state->handoff-status
  "Map from console states to handoff statuses.
   States not in this map do not trigger handoff writes."
  {:running-sdk    :active
   :needs-input    :needs-input
   :running-cli    :active
   :task-complete  :completed
   :story-complete :completed
   :error-recovery :error})

(def ^:dynamic *handoff-path*
  "Path for handoff file. Rebind for testing."
  handoff/default-handoff-path)

(def ^:private states-using-pre-transition-values
  "States where handoff should use pre-transition values.
   Derived from states that map to :completed status, since these
   states clear task/session fields during transition."
  (into #{} (keep (fn [[state status]]
                    (when (= :completed status) state))
                  state->handoff-status)))

(defn- build-handoff-state
  "Build handoff state map from console states for writing.
   Uses post-state by default, but pre-state for transitions that clear fields.
   Returns nil if required fields are missing."
  [pre-state post-state to-state handoff-status]
  (let [source (if (states-using-pre-transition-values to-state)
                 pre-state
                 post-state)
        {:keys [session-id current-task-id story-id]} source]
    (when (and session-id current-task-id story-id)
      {:status handoff-status
       :session-id session-id
       :task-id current-task-id
       :story-id story-id
       :timestamp (java.time.Instant/now)})))

;;; State Predicates

(defn can-transition?
  "Returns true if transitioning from current state to target state is valid.

   Takes either a state map or a state keyword as first argument."
  [state-or-map to-state]
  (let [from-state (if (map? state-or-map)
                     (:state state-or-map)
                     state-or-map)]
    (contains? (get valid-transitions from-state #{}) to-state)))

;;; Context Keys by Transition
;;
;; Different transitions expect different context keys to be provided:
;; - → :selecting-task from :idle: {:story-id id}
;; - → :running-sdk: {:session-id id, :current-task-id id, :cwd path}
;; - → :needs-input: {} (preserves existing)
;; - → :running-cli: {} (preserves existing)
;; - → :error-recovery: {:error error-map}
;; - → :task-complete: {} (clears :current-task-id)
;; - → :story-complete: {} (clears task/session)
;; - → :idle from anywhere: {} (resets to initial)

(defn initial-workspace-state
  "Returns the initial state map structure for a workspace."
  []
  {:state :idle
   :story-id nil
   :current-task-id nil
   :session-id nil
   :cwd nil
   :error nil
   :history []
   :paused false
   :sessions []})

(defn- validate-transition
  "Validates that a transition is legal. Throws ex-info if invalid."
  [from-state to-state]
  (when-not (contains? all-states from-state)
    (throw (ex-info (str "Invalid source state: " from-state)
                    {:type :invalid-state
                     :state from-state
                     :valid-states all-states})))
  (when-not (contains? all-states to-state)
    (throw (ex-info (str "Invalid target state: " to-state)
                    {:type :invalid-state
                     :state to-state
                     :valid-states all-states})))
  (when-not (can-transition? from-state to-state)
    (throw (ex-info (str "Invalid transition: " from-state " → " to-state)
                    {:type :invalid-transition
                     :from-state from-state
                     :to-state to-state
                     :valid-targets (get valid-transitions from-state #{})}))))

(defn- apply-transition-context
  "Applies context to state based on target state.
   Returns updated state map with context fields populated.
   Preserves :history from input state."
  [state to-state context]
  (let [history (:history state [])]
    (case to-state
      :idle
      (-> (initial-workspace-state)
          (assoc :history history)
          (merge (select-keys context [:story-id])))

      :selecting-task
      (-> state
          (assoc :state to-state)
          (merge (select-keys context [:story-id]))
          (assoc :error nil))

      :running-sdk
      (-> state
          (assoc :state to-state)
          (merge (select-keys context [:session-id :current-task-id :cwd]))
          (assoc :error nil))

      :needs-input
      (assoc state :state to-state)

      :running-cli
      (assoc state :state to-state)

      :error-recovery
      (-> state
          (assoc :state to-state)
          (assoc :error (:error context)))

      :task-complete
      (-> state
          (assoc :state to-state)
          (assoc :current-task-id nil))

      :story-complete
      (-> state
          (assoc :state to-state)
          (assoc :current-task-id nil)
          (assoc :session-id nil)))))

;;; Pure Transition Function

(defn transition
  "Transitions state map to a new state, applying context.

   Pure function that returns a new state map or throws on invalid transition.

   Arguments:
   - state: Current state map with at least {:state <keyword>}
   - to-state: Target state keyword
   - context: Map of context to apply during transition

   Context keys used depend on target state:
   - :selecting-task - {:story-id id} (optional, from :idle)
   - :running-sdk - {:session-id id, :current-task-id id}
   - :error-recovery - {:error error-map}

   Returns new state map with :state updated and context applied.

   Throws ex-info with :type :invalid-transition or :invalid-state on error."
  ([state to-state]
   (transition state to-state {}))
  ([state to-state context]
   (let [from-state (:state state)]
     (validate-transition from-state to-state)
     (apply-transition-context state to-state context))))

;;; Mutable State Management

;; Atom holding workspace-keyed state: {workspace-path → state-map}
(defonce console-state
  (atom {}))

;;; Workspace Resolution

(defn workspace-alias->path
  "Resolves a workspace alias keyword to a full path.
   Given :foo, finds the registered project whose path ends with 'foo'.
   Returns nil if no matching project is found."
  [alias]
  (let [alias-name (name alias)
        projects (workspace/list-projects)]
    (first (filter #(= alias-name (last (str/split % #"/"))) projects))))

(defn resolve-workspace
  "Resolves workspace argument to a full path.
   - nil → focused-project
   - keyword → workspace-alias->path
   - string → used as-is"
  [workspace]
  (cond
    (nil? workspace) (workspace/focused-project)
    (keyword? workspace) (workspace-alias->path workspace)
    (string? workspace) workspace))

(defn get-workspace-state
  "Returns the state map for a workspace path.
   Automatically initializes state for new workspaces.
   Workspace can be: nil (focused), keyword alias, or string path."
  [workspace]
  (let [path (resolve-workspace workspace)]
    (get @console-state path (initial-workspace-state))))

(defn- make-history-entry
  "Creates a history entry for a transition."
  [from-state to-state context]
  {:from from-state
   :to to-state
   :timestamp (java.time.Instant/now)
   :context (when (seq context) context)})

(defn- maybe-write-handoff!
  "Write handoff state if target state has a handoff mapping.
   Returns nil, called for side effects only."
  [pre-state post-state to-state]
  (when-let [handoff-status (state->handoff-status to-state)]
    (when-let [handoff-state (build-handoff-state pre-state post-state to-state handoff-status)]
      (handoff/write-handoff-state handoff-state *handoff-path*))))

(defn transition!
  "Transitions console state atom to a new state for a workspace.

   Validates transition, updates atom, appends to history, and writes
   handoff file for states that require it.
   Returns the new state map.

   Arguments:
   - workspace: Workspace path, keyword alias, or nil for focused workspace
   - to-state: Target state keyword
   - context: Optional map of context to apply during transition

   Throws ex-info on invalid transition (see `transition` for details)."
  ([to-state]
   (transition! nil to-state {}))
  ([workspace-or-to-state to-state-or-context]
   (if (keyword? workspace-or-to-state)
     ;; Called as (transition! :to-state context) - backward compat
     (transition! nil workspace-or-to-state to-state-or-context)
     ;; Called as (transition! workspace :to-state)
     (transition! workspace-or-to-state to-state-or-context {})))
  ([workspace to-state context]
   (let [path (resolve-workspace workspace)
         [pre-all-state new-all-state]
         (swap-vals! console-state
                     (fn [all-state]
                       (let [current (get all-state path (initial-workspace-state))
                             from-state (:state current)
                             transitioned (transition current to-state context)
                             entry (make-history-entry from-state to-state context)
                             new-ws-state (update transitioned :history conj entry)]
                         (assoc all-state path new-ws-state))))
         pre-state (get pre-all-state path (initial-workspace-state))
         new-state (get new-all-state path)]
     (maybe-write-handoff! pre-state new-state to-state)
     new-state)))

;;; Query Functions

(defn current-state
  "Returns the current state keyword for a workspace.
   Workspace can be: nil (focused), keyword alias, or string path."
  ([]
   (current-state nil))
  ([workspace]
   (:state (get-workspace-state workspace))))

(defn state-history
  "Returns the history vector of transitions for a workspace.
   Workspace can be: nil (focused), keyword alias, or string path."
  ([]
   (state-history nil))
  ([workspace]
   (:history (get-workspace-state workspace))))

(defn reset-state!
  "Resets the console state.
   With no args: resets all workspaces to empty.
   With workspace arg: resets only that workspace to initial state.
   For testing and dev use."
  ([]
   (reset! console-state {}))
  ([workspace]
   (let [path (resolve-workspace workspace)]
     (swap! console-state dissoc path))))

;;; Pause Control

(defn paused?
  "Returns true if execution is paused for a workspace.
   Workspace can be: nil (focused), keyword alias, or string path."
  ([]
   (paused? nil))
  ([workspace]
   (:paused (get-workspace-state workspace))))

(defn set-paused!
  "Sets the paused flag to true for a workspace.
   Workspace can be: nil (focused), keyword alias, or string path."
  ([]
   (set-paused! nil))
  ([workspace]
   (let [path (resolve-workspace workspace)]
     (swap! console-state update path
            (fn [ws-state]
              (assoc (or ws-state (initial-workspace-state)) :paused true)))
     true)))

(defn clear-paused!
  "Sets the paused flag to false for a workspace.
   Workspace can be: nil (focused), keyword alias, or string path."
  ([]
   (clear-paused! nil))
  ([workspace]
   (let [path (resolve-workspace workspace)]
     (swap! console-state update path
            (fn [ws-state]
              (assoc (or ws-state (initial-workspace-state)) :paused false)))
     false)))

;;; Session Tracking

(defn record-session!
  "Records a session entry to the :sessions vector for a workspace.
   Automatically includes the current story-id for filtering.
   Workspace can be: nil (focused), keyword alias, or string path.
   Returns the updated sessions vector."
  ([session-id task-id]
   (record-session! nil session-id task-id (java.time.Instant/now)))
  ([workspace session-id task-id timestamp]
   (let [path (resolve-workspace workspace)
         story-id (:story-id (get-workspace-state workspace))
         entry {:session-id session-id
                :task-id task-id
                :story-id story-id
                :timestamp timestamp}]
     (swap! console-state update path
            (fn [ws-state]
              (update (or ws-state (initial-workspace-state)) :sessions conj entry)))
     (:sessions (get-workspace-state workspace)))))

(defn update-workspace!
  "Updates arbitrary fields in a workspace's state map.
   Takes workspace (nil/keyword/string) and a map of updates.
   Merges the updates into the workspace's state.
   Workspace can be: nil (focused), keyword alias, or string path.
   Returns the updated workspace state."
  [workspace updates]
  (let [path (resolve-workspace workspace)]
    (swap! console-state update path
           (fn [ws-state]
             (merge (or ws-state (initial-workspace-state)) updates)))
    (get-workspace-state workspace)))

;;; CLI Handoff

(defn launch-cli-resume
  "Launch claude CLI to resume session, blocking until exit.
   Returns process exit code.

   The CLI is launched with inherited stdio so the user interacts
   directly with the terminal. Both working directory and environment
   variables are inherited from the current process by babashka.process
   default behavior. This function blocks until the CLI process exits."
  [session-id]
  (let [proc (p/process ["claude" "--resume" session-id]
                        {:inherit true})]
    (:exit @proc)))

(defn- determine-cli-result
  "Determine the result state and error info based on exit code and hook status.
   Returns {:next-state <keyword> :error <map-or-nil>}."
  [exit-code hook-status]
  (cond
    ;; CLI exited with error
    (not (zero? exit-code))
    {:next-state :error-recovery
     :error {:type :cli-error
             :exit-code exit-code}}

    ;; Hook status indicates error
    (= :error (:status hook-status))
    {:next-state :error-recovery
     :error {:type :hook-error
             :hook-status hook-status}}

    ;; No hook status (user killed CLI or hook didn't run)
    (nil? hook-status)
    {:next-state :error-recovery
     :error {:type :cli-killed
             :reason :cli-killed}}

    ;; Success - hook status is :completed or other non-error status
    :else
    {:next-state :running-sdk
     :error nil}))

(defn hand-to-cli
  "Hand off from SDK to CLI for user interaction.

   Transitions state machine through :needs-input to :running-cli,
   launches the CLI with the current session, and monitors the handoff
   file for status changes.

   Supports two modes:
   1. Blocking (no dev-env): Launches CLI process directly, blocks until exit
   2. Async (with dev-env): Delegates to dev-env, watches file for status

   Arguments:
   - opts: Optional map with:
     :workspace      - Workspace path, keyword alias, or nil for focused
     :dev-env        - DevEnv instance for async operation
     :idle-callback  - Function called when CLI becomes idle (async only)
     :callback       - Function called when session completes (async only)
     :prompt         - Optional prompt to send after session opens (async only)

   Blocking mode return value:
   - :state - the new state map after transition
   - :cli-status - the hook status read from the handoff file (or nil)

   Async mode return value:
   - :status - :running (file watcher active, callbacks will be invoked)

   Status monitoring (async mode):
   - :idle status → invokes idle-callback, stays in :running-cli
   - :completed status → transitions to :running-sdk, invokes callback
   - :error status → transitions to :error-recovery, invokes callback

   After CLI exits (blocking), determines outcome:
   - Exit code 0 + hook status :completed → :running-sdk
   - Exit code non-zero → :error-recovery
   - Hook status :error → :error-recovery
   - Missing hook status (user killed CLI) → :error-recovery with :cli-killed

   Throws if current state is not :running-sdk."
  ([]
   (hand-to-cli {}))
  ([opts]
   (let [{:keys [workspace dev-env callback idle-callback prompt]} opts
         ws-state (get-workspace-state workspace)
         current (:state ws-state)]
     (when (not= :running-sdk current)
       (throw (ex-info (str "hand-to-cli requires :running-sdk state, got " current)
                       {:type :invalid-state
                        :current-state current
                        :required-state :running-sdk})))
     (transition! workspace :needs-input)
     (transition! workspace :running-cli)
     (let [ws-state' (get-workspace-state workspace)
           session-id (:session-id ws-state')
           working-dir (or (:cwd ws-state')
                           (System/getProperty "user.dir"))
           handoff-path (str working-dir "/" handoff/hook-handoff-path)]
       (if dev-env
         ;; Async mode: watch file for status changes, delegate to dev-env
         (let [stop-watch-fn (atom nil)
               completed? (atom false)
               ;; Shared completion handler - only fires once
               handle-completion (fn [source hook-status]
                                   (when (compare-and-set! completed? false true)
                                     (println "[hand-to-cli] Completion detected via" source)
                                     (when-let [stop-fn @stop-watch-fn]
                                       (stop-fn))
                                     (let [new-state (if (= :error (:status hook-status))
                                                       (transition! workspace :error-recovery
                                                                    {:error {:type :hook-error
                                                                             :hook-status hook-status}})
                                                       (transition! workspace :running-sdk))]
                                       (when callback
                                         (callback {:state new-state
                                                    :cli-status hook-status})))))
               watcher-callback (fn [hook-status]
                                  (case (:status hook-status)
                                    :idle (when idle-callback (idle-callback hook-status))
                                    :completed (handle-completion :file-watcher hook-status)
                                    :error (handle-completion :file-watcher hook-status)
                                    nil))
               emacs-callback (fn [result]
                                (println "[hand-to-cli] Emacs callback fired:" (pr-str result))
                                (handle-completion :emacs-callback
                                                   {:status (if (= :completed (:status result))
                                                              :completed
                                                              :error)}))]
           ;; Start watching handoff file for status changes (in working dir)
           (reset! stop-watch-fn
                   (handoff/watch-hook-status-file watcher-callback handoff-path))
           ;; Open CLI session with Emacs callback as backup
           (dev-env/open-cli-session
            dev-env
            {:session-id session-id
             :prompt prompt
             :working-dir working-dir}
            emacs-callback)
           {:status :running})
         ;; Blocking mode: existing behavior
         (let [exit-code (launch-cli-resume session-id)
               cli-status (handoff/read-hook-status handoff-path)
               {:keys [error]} (determine-cli-result exit-code cli-status)
               new-state (if error
                           (transition! workspace :error-recovery {:error error})
                           (transition! workspace :running-sdk))]
           {:state new-state
            :cli-status cli-status}))))))
