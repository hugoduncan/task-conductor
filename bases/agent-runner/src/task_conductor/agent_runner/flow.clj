(ns task-conductor.agent-runner.flow
  "Flow model protocol for controlling the orchestration loop.

   Defines the FlowModel protocol that determines how the orchestrator
   responds to events (CLI return, SDK completion, task completion) and
   generates prompts. Enables pluggable execution strategies.

   The FlowDecision schema defines the return contract for all flow model
   methods, specifying the next action and any associated data."
  (:require
   [malli.core :as m]
   [malli.error :as me]))

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

   The schema is open to allow extension by custom flow models."
  [:map {:closed false}
   [:action Action]
   [:prompt {:optional true} :string]
   [:reason {:optional true} :string]
   [:context {:optional true} :map]])

;;; FlowDecision Validation

(defn valid-decision?
  "Returns true if the decision map is valid against FlowDecision schema."
  [decision]
  (m/validate FlowDecision decision))

(defn explain-decision
  "Returns human-readable explanation of validation errors, or nil if valid."
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

;;; DefaultFlowModel Implementation

(defn- query-next-task
  "Query mcp-tasks for next unblocked child of story.
   Returns the first unblocked task or nil if none available."
  [run-mcp-tasks-fn story-id]
  (let [{:keys [tasks]} (run-mcp-tasks-fn "list"
                                          "--parent-id" (str story-id)
                                          "--blocked" "false"
                                          "--limit" "1")]
    (first tasks)))

(defrecord DefaultFlowModel [run-mcp-tasks-fn]
  FlowModel

  (initial-prompt [_this task-info _console-state]
    (let [parent-id (:parent-id task-info)]
      {:action :continue-sdk
       :prompt (format "/mcp-tasks:execute-story-child %d" parent-id)}))

  (on-cli-return [_this cli-status _console-state]
    ;; Simple resume prompt. Task #121 will add build-resume-prompt helper.
    (let [status (:status cli-status)
          reason (or (:reason cli-status) (:question cli-status))]
      {:action :continue-sdk
       :prompt (if reason
                 (format "CLI returned with status %s: %s. Continue the task."
                         (name status) reason)
                 (format "CLI returned with status %s. Continue the task."
                         (name status)))}))

  (on-sdk-complete [_this _sdk-result _console-state]
    ;; Simple strategy: SDK completion means task is done.
    ;; No "needs input" detection in this implementation.
    {:action :task-done})

  (on-task-complete [_this console-state]
    (let [story-id (:story-id console-state)
          next-task (query-next-task run-mcp-tasks-fn story-id)]
      (if next-task
        {:action :continue-sdk
         :prompt (format "/mcp-tasks:execute-story-child %d" story-id)}
        {:action :story-done
         :reason "All tasks complete"}))))

(defn default-flow-model
  "Create a DefaultFlowModel with the given mcp-tasks query function.

   run-mcp-tasks-fn should have the signature of orchestrator/run-mcp-tasks:
   (run-mcp-tasks-fn & args) -> parsed EDN result from mcp-tasks CLI."
  [run-mcp-tasks-fn]
  (->DefaultFlowModel run-mcp-tasks-fn))
