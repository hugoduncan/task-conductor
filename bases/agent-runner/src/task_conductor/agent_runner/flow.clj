(ns task-conductor.agent-runner.flow
  "Flow model protocol for controlling the orchestration loop.

   Defines the FlowModel protocol that determines how the orchestrator
   responds to events (CLI return, SDK completion, task completion) and
   generates prompts. Enables pluggable execution strategies.

   The FlowDecision schema defines the return contract for all flow model
   methods, specifying the next action and any associated data."
  (:require
   [clojure.string :as str]
   [malli.core :as m]
   [malli.error :as me]))

;;; Story State

(def StoryState
  "Story state keywords derived from task fields and child tasks.
   States are computed, not stored, following the flow:
   unrefined → refined → execute-tasks ⟷ code-review → create-pr →
   manual-review → merge-pr → story-complete"
  [:enum :unrefined-story :refined :execute-tasks :code-review
   :create-pr :manual-review :merge-pr :story-complete])

(defn derive-story-state
  "Derive story state from story task and its children. Pure function.

   Arguments:
   - story: Story task map with :meta, :status
   - children: Collection of child task maps (may be empty)

   Returns one of the StoryState keywords.

   Precedence (evaluated in order):
   1. Story closed with :pr-merged? true → :story-complete
   2. :ready-to-merge in meta → :merge-pr
   3. Has :pr-num → :manual-review
   4. Has :code-reviewed, all tasks complete, no :pr-num → :create-pr
   5. All tasks complete, no :code-reviewed → :code-review
   6. Has incomplete child tasks → :execute-tasks
   7. Has refined: true but no children → :refined
   8. No refined: true → :unrefined-story"
  [story children]
  (let [meta-map (or (:meta story) {})
        refined? (get meta-map :refined)
        code-reviewed (get meta-map :code-reviewed)
        pr-num (get meta-map :pr-num)
        pr-merged? (get meta-map :pr-merged?)
        ready-to-merge (get meta-map :ready-to-merge)
        has-children? (seq children)
        all-complete? (and has-children?
                           (every? #(= :closed (:status %)) children))]
    (cond
      ;; Precedence 1: PR merged
      pr-merged?
      :story-complete

      ;; Precedence 2: Ready to merge (user signals via CLI)
      ready-to-merge
      :merge-pr

      ;; Precedence 3: Has PR number, waiting for manual review
      pr-num
      :manual-review

      ;; Precedence 4: Code reviewed, all complete, need PR
      (and code-reviewed all-complete? (not pr-num))
      :create-pr

      ;; Precedence 5: All complete, need code review
      (and all-complete? (not code-reviewed))
      :code-review

      ;; Precedence 6: Has incomplete children, execute tasks
      (and has-children? (not all-complete?))
      :execute-tasks

      ;; Precedence 7: Refined but no children yet
      (and refined? (not has-children?))
      :refined

      ;; Precedence 8: Not refined
      :else
      :unrefined-story)))

;;; FlowDecision Schema

(def Action
  "Valid action keywords for FlowDecision.
   - :continue-sdk - Continue with SDK, :prompt required
   - :hand-to-cli - Hand off to CLI for user interaction
   - :task-done - Current task is complete
   - :story-done - Story execution is complete
   - :error - An error occurred, :reason recommended
   - :pause - Pause execution"
  [:enum :continue-sdk :hand-to-cli :task-done :story-done :error :pause])

(def FlowDecision
  "Schema for flow model method return values.

   Required fields:
   - :action - keyword indicating next action

   Conditional fields:
   - :prompt - string for SDK (required when :action is :continue-sdk)

   Optional fields:
   - :reason - string explaining the decision
   - :context - map for state machine transitions

   The schema is open to allow extension by custom flow models.

   Note: When :prompt is present, it must be a non-nil string. The :string
   schema rejects nil - use absence of key rather than nil when prompt
   is not needed. Malli's {:optional true} only controls key presence,
   not nil acceptance."
  [:map {:closed false}
   [:action Action]
   [:prompt {:optional true} [:string {:min 1}]]
   [:reason {:optional true} :string]
   [:context {:optional true} :map]])

;;; FlowDecision Validation

(defn valid-decision?
  "Returns true if the decision map is valid against FlowDecision schema."
  [decision]
  (m/validate FlowDecision decision))

(defn explain-decision
  "Returns a humanized error map for validation failures, or nil if valid.
   The returned map has the same shape as the input with error messages at
   failing paths (via malli.error/humanize)."
  [decision]
  (when-let [explanation (m/explain FlowDecision decision)]
    (me/humanize explanation)))

(defn validate-decision!
  "Validate decision against FlowDecision schema. Throws on invalid.
   Returns the decision unchanged if valid."
  [decision]
  (when-let [errors (explain-decision decision)]
    (throw (ex-info (str "Invalid FlowDecision: " (pr-str errors))
                    {:type :validation-error
                     :errors errors
                     :decision decision})))
  decision)

(defn validate-continue-sdk-prompt!
  "Validate that :continue-sdk decisions have a :prompt.
   Returns the decision unchanged if valid.
   Throws if :action is :continue-sdk but :prompt is missing or empty."
  [decision]
  (when (and (= :continue-sdk (:action decision))
             (empty? (:prompt decision)))
    (throw (ex-info "FlowDecision with :continue-sdk action requires non-empty :prompt"
                    {:type :validation-error
                     :decision decision})))
  decision)

(defn validate-decision-complete!
  "Full validation of a FlowDecision including semantic checks.
   Validates schema and semantic constraints (e.g., :prompt required for :continue-sdk).
   Returns the decision unchanged if valid."
  [decision]
  (-> decision
      validate-decision!
      validate-continue-sdk-prompt!))

;;; FlowModel Protocol

(defprotocol FlowModel
  "Protocol for controlling the orchestration loop.

   Flow models decide what action to take after each event in the
   execution loop. All methods return a FlowDecision map.

   Implementations can vary from simple linear execution to complex
   interactive or autonomous strategies."

  (on-cli-return [this cli-status console-state]
    "Called after CLI handoff returns.

     Arguments:
     - cli-status: HookStatus map from handoff file
       (:status, :timestamp, optional :reason/:question)
     - console-state: Full console state map

     Returns FlowDecision, typically :continue-sdk with a resume prompt.")

  (on-sdk-complete [this sdk-result console-state]
    "Called after SDK turn completes.

     Arguments:
     - sdk-result: Result from SDK query (:messages, :result)
     - console-state: Full console state map

     Returns FlowDecision, typically :task-done or :hand-to-cli.")

  (on-task-complete [this console-state]
    "Called when a task finishes.

     Arguments:
     - console-state: Full console state map

     Returns FlowDecision indicating whether to continue with
     next task (:continue-sdk), finish (:story-done), or other action.")

  (initial-prompt [this task-info console-state]
    "Generate the first prompt for a new task.

     Arguments:
     - task-info: Task map from mcp-tasks
     - console-state: Full console state map

     Returns FlowDecision with :action :continue-sdk and :prompt."))

;;; Prompt Generation Helpers

(defn build-resume-prompt
  "Generate SDK prompt from CLI status and optional shared-context.

   Arguments:
   - cli-status: HookStatus map with :status and optional :reason/:question
   - shared-context: Optional vector of context strings from previous tasks

   Returns a string prompt instructing the SDK to continue the task,
   incorporating CLI status information and any relevant context."
  ([cli-status]
   (build-resume-prompt cli-status nil))
  ([cli-status shared-context]
   (let [status (:status cli-status)
         reason (or (:reason cli-status) (:question cli-status))
         status-msg (if reason
                      (format "CLI returned with status %s: %s"
                              (name status) reason)
                      (format "CLI returned with status %s"
                              (name status)))
         context-msg (when (seq shared-context)
                       (str "\n\nContext from previous tasks:\n"
                            (str/join "\n" shared-context)))]
     (str status-msg
          (or context-msg "")
          "\n\nContinue the task."))))

;;; DefaultFlowModel Implementation

(defn- query-next-task
  "Query mcp-tasks for next unblocked child of story.
   Returns {:ok task} for success (task may be nil if none available),
   or {:error message} on failure."
  [run-mcp-tasks-fn story-id]
  (let [result (run-mcp-tasks-fn "list"
                                 "--parent-id" (str story-id)
                                 "--blocked" "false"
                                 "--limit" "1")]
    (cond
      ;; Check for explicit error in response
      (:error result)
      {:error (:error result)}

      ;; Valid response with tasks key
      (contains? result :tasks)
      {:ok (first (:tasks result))}

      ;; Unexpected response format
      :else
      {:error (str "Unexpected mcp-tasks response format: " (pr-str result))})))

(defrecord DefaultFlowModel [run-mcp-tasks-fn]
  FlowModel

  (initial-prompt [_this task-info _console-state]
    (let [parent-id (:parent-id task-info)]
      {:action :continue-sdk
       :prompt (format "/mcp-tasks:execute-story-child %d" parent-id)}))

  (on-cli-return [_this cli-status console-state]
    {:action :continue-sdk
     :prompt (build-resume-prompt cli-status (:shared-context console-state))})

  (on-sdk-complete [_this _sdk-result _console-state]
    ;; Simple strategy: SDK completion means task is done.
    ;; No "needs input" detection in this implementation.
    {:action :task-done})

  (on-task-complete [_this console-state]
    (let [story-id (:story-id console-state)
          result (query-next-task run-mcp-tasks-fn story-id)]
      (cond
        (:error result)
        {:action :error
         :reason (:error result)}

        (:ok result)
        {:action :continue-sdk
         :prompt (format "/mcp-tasks:execute-story-child %d" story-id)}

        :else
        {:action :story-done
         :reason "All tasks complete"}))))

(defn default-flow-model
  "Create a DefaultFlowModel with the given mcp-tasks query function.

   run-mcp-tasks-fn should have the signature of orchestrator/run-mcp-tasks:
   (run-mcp-tasks-fn & args) -> parsed EDN result from mcp-tasks CLI."
  [run-mcp-tasks-fn]
  (->DefaultFlowModel run-mcp-tasks-fn))

;;; StoryFlowModel Implementation

(defn- query-story-and-children
  "Query mcp-tasks for story and its children.
   Returns {:ok {:story story :children children}} or {:error message}."
  [run-mcp-tasks-fn story-id]
  (let [story-result (run-mcp-tasks-fn "list"
                                       "--task-id" (str story-id)
                                       "--unique" "true")
        children-result (run-mcp-tasks-fn "list"
                                          "--parent-id" (str story-id))]
    (cond
      (:error story-result)
      {:error (:error story-result)}

      (:error children-result)
      {:error (:error children-result)}

      (not (contains? story-result :tasks))
      {:error (str "Unexpected story response format: " (pr-str story-result))}

      (not (contains? children-result :tasks))
      {:error (str "Unexpected children response format: " (pr-str children-result))}

      (empty? (:tasks story-result))
      {:error (str "Story not found: " story-id)}

      :else
      {:ok {:story (first (:tasks story-result))
            :children (:tasks children-result)}})))

(defn state->flow-decision
  "Map story state to FlowDecision. Pure function.

   Arguments:
   - state: StoryState keyword from derive-story-state
   - story-id: Story task ID for prompt generation

   Returns FlowDecision with appropriate :action and :prompt/:reason."
  [state story-id]
  (case state
    :unrefined-story
    {:action :continue-sdk
     :prompt (format "/mcp-tasks:refine-task %d" story-id)}

    :refined
    {:action :continue-sdk
     :prompt (format "/mcp-tasks:create-story-children %d" story-id)}

    :execute-tasks
    {:action :continue-sdk
     :prompt (format "/mcp-tasks:execute-story-child %d" story-id)}

    :code-review
    {:action :continue-sdk
     :prompt (format "/mcp-tasks:review-story-implementation %d" story-id)}

    :create-pr
    {:action :continue-sdk
     :prompt (format "/mcp-tasks:create-story-pr %d" story-id)}

    :manual-review
    {:action :hand-to-cli
     :reason "PR created and awaiting manual review"}

    :merge-pr
    {:action :hand-to-cli
     :reason "PR approved and ready to merge"}

    :story-complete
    {:action :story-done
     :reason "Story complete - PR merged"}))

(defn- derive-flow-decision
  "Query story and children, derive state, return FlowDecision.
   Encapsulates the common pattern used by all StoryFlowModel methods."
  [run-mcp-tasks-fn story-id]
  (let [result (query-story-and-children run-mcp-tasks-fn story-id)]
    (if (:error result)
      {:action :error
       :reason (:error result)}
      (let [{:keys [story children]} (:ok result)
            state (derive-story-state story children)]
        (state->flow-decision state story-id)))))

(defrecord StoryFlowModel [run-mcp-tasks-fn story-id]
  FlowModel

  (initial-prompt [_this _task-info _console-state]
    (derive-flow-decision run-mcp-tasks-fn story-id))

  (on-cli-return [_this _cli-status _console-state]
    (derive-flow-decision run-mcp-tasks-fn story-id))

  (on-sdk-complete [_this _sdk-result _console-state]
    (derive-flow-decision run-mcp-tasks-fn story-id))

  (on-task-complete [_this _console-state]
    (derive-flow-decision run-mcp-tasks-fn story-id)))

(defn story-flow-model
  "Create a StoryFlowModel for state-driven story execution.

   Arguments:
   - run-mcp-tasks-fn: Function to query mcp-tasks CLI
   - story-id: ID of the story task to execute

   The model derives state from story fields and children, then returns
   appropriate FlowDecisions for each lifecycle phase."
  [run-mcp-tasks-fn story-id]
  (->StoryFlowModel run-mcp-tasks-fn story-id))
