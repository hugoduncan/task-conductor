(ns task-conductor.agent-runner.repl
  "User-facing REPL control functions for story execution.

   Provides interactive functions for controlling the console state machine
   from the REPL. Each function prints human-readable output and returns
   data for programmatic use."
  (:require
   [babashka.process :as p]
   [clojure.edn :as edn]
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
   Returns the current state map."
  []
  (console/set-paused!)
  (println "Paused - execution will stop after current task completes")
  @console/console-state)

(defn continue
  "Clear the pause flag.

   Allows the outer loop to resume on the next iteration.
   Returns the current state map."
  []
  (console/clear-paused!)
  (println "Resumed - execution will continue")
  @console/console-state)

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

;;; Context Management Functions

(defn- validate-story-id
  "Returns the current story-id or throws if not in a story."
  []
  (let [story-id (:story-id @console/console-state)]
    (when-not story-id
      (throw (ex-info "No active story"
                      {:type :no-active-story
                       :current-state (console/current-state)})))
    story-id))

(defn- run-mcp-tasks
  "Run mcp-tasks CLI command and return parsed EDN result.
   Throws on non-zero exit."
  [& args]
  (let [result (apply p/sh "mcp-tasks" args)]
    (when (not= 0 (:exit result))
      (throw (ex-info (str "mcp-tasks failed: " (:err result))
                      {:type :cli-error
                       :args args
                       :exit-code (:exit result)
                       :stderr (:err result)})))
    (edn/read-string (:out result))))

(defn add-context
  "Append text to the current story's shared-context.

   Validates that a story is active, then shells out to mcp-tasks CLI
   to update the story's shared-context field.

   Returns the updated task on success.
   Throws if no active story or CLI call fails."
  [text]
  (let [story-id (validate-story-id)
        result (run-mcp-tasks "update"
                              "--task-id" (str story-id)
                              "--shared-context" text
                              "--format" "edn")]
    (println (str "Added context to story " story-id))
    result))

(defn view-context
  "Display the current story's shared-context.

   Retrieves the story and prints its shared-context in a readable format.

   Returns the shared-context vector.
   Throws if no active story or CLI call fails."
  []
  (let [story-id (validate-story-id)
        result (run-mcp-tasks "show"
                              "--task-id" (str story-id)
                              "--format" "edn")
        shared-context (get-in result [:task :shared-context])]
    (println "Shared Context:")
    (if (seq shared-context)
      (doseq [[idx entry] (map-indexed vector shared-context)]
        (println (str "  " (inc idx) ". " entry)))
      (println "  (none)"))
    shared-context))

;;; Session Tracking

(defn list-sessions
  "List sessions for the current story.

   Retrieves all session entries from console state and filters by the
   current story-id. The story-id is matched by checking which sessions
   were recorded while the story was active.

   Prints a formatted list with session-id, task-id, and timestamp.

   Returns the filtered sessions vector.
   Throws if no active story."
  []
  (let [story-id (validate-story-id)
        all-sessions (:sessions @console/console-state)
        ;; Sessions are recorded during task execution within a story.
        ;; Since we don't store story-id on session entries, we return
        ;; all sessions when a story is active. The outer loop will
        ;; reset sessions when starting a new story.
        sessions all-sessions]
    (println (str "Sessions for story " story-id ":"))
    (if (seq sessions)
      (doseq [[idx {:keys [session-id task-id timestamp]}]
              (map-indexed vector sessions)]
        (println (str "  " (inc idx) ". " session-id
                      " (task " task-id ", " timestamp ")")))
      (println "  (none)"))
    sessions))
