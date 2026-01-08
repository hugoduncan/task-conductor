(ns task-conductor.agent-runner.console
  "Console state machine for orchestrating story execution.

   Manages workflow state transitions between SDK agent execution,
   CLI handoff, and task completion. This namespace provides pure
   transition functions and mutable wrappers with history tracking."
  (:require
   [babashka.process :as p]
   [task-conductor.agent-runner.handoff :as handoff]))

;;; State Machine Definition

(def valid-transitions
  "Map of valid state transitions.
   Each key is a source state, value is set of valid target states."
  {:idle            #{:selecting-task}
   :selecting-task  #{:running-sdk :story-complete}
   :running-sdk     #{:needs-input :task-complete :error-recovery}
   :needs-input     #{:running-cli}
   :running-cli     #{:running-sdk :error-recovery}
   :error-recovery  #{:selecting-task :idle}
   :task-complete   #{:selecting-task :story-complete}
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
;; - → :running-sdk: {:session-id id, :current-task-id id}
;; - → :needs-input: {} (preserves existing)
;; - → :running-cli: {} (preserves existing)
;; - → :error-recovery: {:error error-map}
;; - → :task-complete: {} (clears :current-task-id)
;; - → :story-complete: {} (clears task/session)
;; - → :idle from anywhere: {} (resets to initial)

(def initial-state
  "Initial state map structure."
  {:state :idle
   :story-id nil
   :current-task-id nil
   :session-id nil
   :error nil
   :history []})

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
      (-> initial-state
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
          (merge (select-keys context [:session-id :current-task-id]))
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

(defonce console-state
  (atom initial-state))

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
  "Transitions console state atom to a new state.

   Validates transition, updates atom, appends to history, and writes
   handoff file for states that require it.
   Returns the new state map.

   Arguments:
   - to-state: Target state keyword
   - context: Optional map of context to apply during transition

   Throws ex-info on invalid transition (see `transition` for details)."
  ([to-state]
   (transition! to-state {}))
  ([to-state context]
   (let [[pre-state new-state]
         (swap-vals! console-state
                     (fn [current]
                       (let [from-state (:state current)
                             transitioned (transition current to-state context)
                             entry (make-history-entry from-state to-state context)]
                         (update transitioned :history conj entry))))]
     (maybe-write-handoff! pre-state new-state to-state)
     new-state)))

;;; Query Functions

(defn current-state
  "Returns the current state keyword."
  []
  (:state @console-state))

(defn state-history
  "Returns the history vector of transitions."
  []
  (:history @console-state))

(defn reset-state!
  "Resets the console state to initial values.
   Clears history. For testing and dev use."
  []
  (reset! console-state initial-state))

;;; CLI Handoff

(defn launch-cli-resume
  "Launch claude CLI to resume session, blocking until exit.
   Returns process exit code.

   The CLI is launched with inherited stdio so the user interacts
   directly with the terminal. This function blocks until the CLI
   process exits."
  [session-id]
  (let [proc (p/process ["claude" "--resume" session-id]
                        {:inherit true})]
    (:exit @proc)))

(defn hand-to-cli
  "Hand off from SDK to CLI for user interaction.

   Transitions state machine through :needs-input to :running-cli,
   launches the CLI with the current session, and handles the exit.

   On CLI exit code 0: transitions back to :running-sdk
   On non-zero exit: transitions to :error-recovery

   Returns the new state map after CLI exits.

   Throws if current state is not :running-sdk."
  []
  (let [current (:state @console-state)]
    (when (not= :running-sdk current)
      (throw (ex-info (str "hand-to-cli requires :running-sdk state, got " current)
                      {:type :invalid-state
                       :current-state current
                       :required-state :running-sdk}))))
  (transition! :needs-input)
  (transition! :running-cli)
  (let [session-id (:session-id @console-state)
        exit-code (launch-cli-resume session-id)]
    (if (zero? exit-code)
      (transition! :running-sdk)
      (transition! :error-recovery
                   {:error {:type :cli-error
                            :exit-code exit-code}}))))
