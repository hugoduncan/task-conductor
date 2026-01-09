(ns task-conductor.agent-runner.repl
  "User-facing REPL control functions for story execution.

   Provides interactive functions for controlling the console state machine
   from the REPL. Each function prints human-readable output and returns
   data for programmatic use."
  (:require
   [task-conductor.agent-runner.console :as console]))

;;; Control Functions

(defn start-story
  "Begin executing a story's tasks.

   Validates that the console is in :idle state, then transitions to
   :selecting-task with the given story-id.

   Returns the new state map on success.
   Throws if not in :idle state."
  [story-id]
  (let [current (console/current-state)]
    (when (not= :idle current)
      (throw (ex-info (str "Cannot start story: console is " current ", expected :idle")
                      {:type :invalid-state
                       :current-state current
                       :required-state :idle})))
    (let [new-state (console/transition! :selecting-task {:story-id story-id})]
      (println (str "Started story " story-id))
      new-state)))

(defn status
  "Return current console status.

   Returns a map with :state, :story-id, :current-task-id, and :paused.
   Prints a human-readable summary."
  []
  (let [state @console/console-state
        result {:state (:state state)
                :story-id (:story-id state)
                :current-task-id (:current-task-id state)
                :paused (:paused state)}]
    (println (str "State: " (:state result)))
    (when (:story-id result)
      (println (str "Story: " (:story-id result))))
    (when (:current-task-id result)
      (println (str "Task: " (:current-task-id result))))
    (when (:paused result)
      (println "PAUSED"))
    result))

(defn pause
  "Set the pause flag.

   The outer loop checks this flag before starting the next task.
   Returns true."
  []
  (console/set-paused!)
  (println "Paused - execution will stop after current task completes")
  true)

(defn continue
  "Clear the pause flag.

   Allows the outer loop to resume on the next iteration.
   Returns false."
  []
  (console/clear-paused!)
  (println "Resumed - execution will continue")
  false)

(defn abort
  "Cancel execution and return to :idle state.

   Transitions through :error-recovery to :idle, preserving history.
   Returns the new state map.

   If already :idle, prints a message and returns the current state.
   If in :story-complete, transitions directly to :idle."
  []
  (let [current (console/current-state)]
    (cond
      (= :idle current)
      (do
        (println "Already idle")
        @console/console-state)

      (= :story-complete current)
      (let [new-state (console/transition! :idle)]
        (println "Aborted - returned to idle")
        new-state)

      :else
      (do
        (console/transition! :error-recovery {:error {:type :user-abort}})
        (let [new-state (console/transition! :idle)]
          (println "Aborted - returned to idle")
          new-state)))))

;;; Error Recovery Functions

(defn retry
  "Re-attempt the failed task.

   Transitions from :error-recovery back to :running-sdk with the same task.
   Throws if not in :error-recovery state.

   Returns the new state map."
  []
  (let [current (console/current-state)]
    (when (not= :error-recovery current)
      (throw (ex-info (str "Cannot retry: console is " current ", expected :error-recovery")
                      {:type :invalid-state
                       :current-state current
                       :required-state :error-recovery})))
    (let [state @console/console-state
          task-id (:current-task-id state)
          session-id (:session-id state)
          new-state (console/transition! :running-sdk {:session-id session-id
                                                       :current-task-id task-id})]
      (println (str "Retrying task " task-id))
      new-state)))

(defn skip
  "Skip the failed task and move to the next.

   Transitions from :error-recovery to :selecting-task.
   Throws if not in :error-recovery state.

   Returns the new state map."
  []
  (let [current (console/current-state)]
    (when (not= :error-recovery current)
      (throw (ex-info (str "Cannot skip: console is " current ", expected :error-recovery")
                      {:type :invalid-state
                       :current-state current
                       :required-state :error-recovery})))
    (let [state @console/console-state
          task-id (:current-task-id state)
          new-state (console/transition! :selecting-task)]
      (println (str "Skipped task " task-id))
      new-state)))
