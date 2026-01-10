(ns task-conductor.agent-runner.repl-test
  ;; Tests the REPL control functions for story execution.
  ;; Verifies:
  ;; - start-story validates :idle state and transitions to :selecting-task
  ;; - status returns correct map and prints summary
  ;; - pause/continue set/clear the paused flag
  ;; - abort transitions through :error-recovery to :idle
  ;; - retry re-attempts failed task from :error-recovery
  ;; - skip moves to next task from :error-recovery
  ;; - add-context validates story and calls mcp-tasks CLI with correct args
  ;; - view-context retrieves and formats shared-context from mcp-tasks CLI
  ;; - list-sessions filters sessions by current story-id
  ;; - run-story validates :idle state, calls orchestrator, prints output per outcome
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.console :as console]
   [task-conductor.agent-runner.orchestrator :as orchestrator]
   [task-conductor.agent-runner.repl :as repl]))

;;; start-story Tests

(deftest start-story-test
  (testing "start-story"
    (testing "from :idle state"
      (testing "transitions to :selecting-task"
        (console/reset-state!)
        (repl/start-story 53)
        (is (= :selecting-task (console/current-state))))

      (testing "sets story-id"
        (console/reset-state!)
        (repl/start-story 53)
        (is (= 53 (:story-id @console/console-state))))

      (testing "returns new state map"
        (console/reset-state!)
        (let [result (repl/start-story 53)]
          (is (map? result))
          (is (= :selecting-task (:state result)))
          (is (= 53 (:story-id result)))))

      (testing "prints confirmation"
        (console/reset-state!)
        (let [output (with-out-str (repl/start-story 53))]
          (is (re-find #"Started story 53" output)))))

    (testing "from non-idle state"
      (testing "throws with informative error"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 42})
        (let [ex (try
                   (repl/start-story 53)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :invalid-state (:type (ex-data ex))))
          (is (= :selecting-task (:current-state (ex-data ex))))
          (is (= :idle (:required-state (ex-data ex)))))))))

;;; status Tests

(deftest status-test
  (testing "status"
    (testing "returns map with required keys"
      (console/reset-state!)
      (let [result (repl/status)]
        (is (contains? result :state))
        (is (contains? result :story-id))
        (is (contains? result :current-task-id))
        (is (contains? result :paused))))

    (testing "returns correct values for :idle state"
      (console/reset-state!)
      (let [result (repl/status)]
        (is (= :idle (:state result)))
        (is (nil? (:story-id result)))
        (is (nil? (:current-task-id result)))
        (is (false? (:paused result)))))

    (testing "returns correct values for :running-sdk state"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "sess-1"
                                         :current-task-id 75})
      (let [result (repl/status)]
        (is (= :running-sdk (:state result)))
        (is (= 53 (:story-id result)))
        (is (= 75 (:current-task-id result)))))

    (testing "reflects paused state"
      (console/reset-state!)
      (console/set-paused!)
      (let [result (repl/status)]
        (is (true? (:paused result)))))

    (testing "prints state"
      (console/reset-state!)
      (let [output (with-out-str (repl/status))]
        (is (re-find #"State: :idle" output))))

    (testing "prints story-id when present"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (let [output (with-out-str (repl/status))]
        (is (re-find #"Story: 53" output))))

    (testing "prints task-id when present"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "s1"
                                         :current-task-id 75})
      (let [output (with-out-str (repl/status))]
        (is (re-find #"Task: 75" output))))

    (testing "prints PAUSED when paused"
      (console/reset-state!)
      (console/set-paused!)
      (let [output (with-out-str (repl/status))]
        (is (re-find #"PAUSED" output))))))

;;; pause Tests

(deftest pause-test
  (testing "pause"
    (testing "sets paused flag to true"
      (console/reset-state!)
      (repl/pause)
      (is (true? (console/paused?))))

    (testing "returns current state map"
      (console/reset-state!)
      (let [result (repl/pause)]
        (is (map? result))
        (is (true? (:paused result)))))

    (testing "prints confirmation"
      (console/reset-state!)
      (let [output (with-out-str (repl/pause))]
        (is (re-find #"Paused" output))))))

;;; continue Tests

(deftest continue-test
  (testing "continue"
    (testing "clears paused flag"
      (console/reset-state!)
      (console/set-paused!)
      (repl/continue)
      (is (false? (console/paused?))))

    (testing "returns current state map"
      (console/reset-state!)
      (console/set-paused!)
      (let [result (repl/continue)]
        (is (map? result))
        (is (false? (:paused result)))))

    (testing "prints confirmation"
      (console/reset-state!)
      (console/set-paused!)
      (let [output (with-out-str (repl/continue))]
        (is (re-find #"Resumed" output))))))

;;; abort Tests

(deftest abort-test
  (testing "abort"
    (testing "from :idle state"
      (testing "returns current state"
        (console/reset-state!)
        (let [result (repl/abort)]
          (is (= :idle (:state result)))))

      (testing "prints already idle message"
        (console/reset-state!)
        (let [output (with-out-str (repl/abort))]
          (is (re-find #"Already idle" output)))))

    (testing "from :story-complete state"
      (testing "transitions directly to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :story-complete)
        (repl/abort)
        (is (= :idle (console/current-state))))

      (testing "prints aborted message"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :story-complete)
        (let [output (with-out-str (repl/abort))]
          (is (re-find #"Aborted" output)))))

    (testing "from :selecting-task state"
      (testing "transitions through :error-recovery to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (repl/abort)
        (is (= :idle (console/current-state))))

      (testing "records :user-abort error"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (repl/abort)
        (let [history (console/state-history)
              error-entry (first (filter #(= :error-recovery (:to %)) history))]
          (is (some? error-entry))
          (is (= :user-abort (get-in error-entry [:context :error :type]))))))

    (testing "from :running-sdk state"
      (testing "transitions to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "s1"
                                           :current-task-id 75})
        (repl/abort)
        (is (= :idle (console/current-state)))))

    (testing "from :needs-input state"
      (testing "transitions to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "s1"
                                           :current-task-id 75})
        (console/transition! :needs-input)
        (repl/abort)
        (is (= :idle (console/current-state)))))

    (testing "from :task-complete state"
      (testing "transitions to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "s1"
                                           :current-task-id 75})
        (console/transition! :task-complete)
        (repl/abort)
        (is (= :idle (console/current-state)))))

    (testing "preserves history"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "s1"
                                         :current-task-id 75})
      (repl/abort)
      (let [history (console/state-history)]
        (is (>= (count history) 4))))

    (testing "clears the paused flag"
      (testing "when paused and aborting from active state"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/set-paused!)
        (is (true? (console/paused?)))
        (repl/abort)
        (is (false? (console/paused?))))

      (testing "when paused and aborting from :story-complete"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :story-complete)
        (console/set-paused!)
        (is (true? (console/paused?)))
        (repl/abort)
        (is (false? (console/paused?)))))))

;;; retry Tests

(defn- setup-error-recovery-state
  "Helper to set up a state machine in :error-recovery with a task."
  [story-id session-id task-id]
  (console/reset-state!)
  (console/transition! :selecting-task {:story-id story-id})
  (console/transition! :running-sdk {:session-id session-id
                                     :current-task-id task-id})
  (console/transition! :error-recovery {:error {:type :test-error}}))

(deftest retry-test
  (testing "retry"
    (testing "from :error-recovery state"
      (testing "transitions to :running-sdk"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/retry)
        (is (= :running-sdk (console/current-state))))

      (testing "preserves task-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/retry)
        (is (= 75 (:current-task-id @console/console-state))))

      (testing "preserves session-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/retry)
        (is (= "sess-1" (:session-id @console/console-state))))

      (testing "returns new state map"
        (setup-error-recovery-state 53 "sess-1" 75)
        (let [result (repl/retry)]
          (is (map? result))
          (is (= :running-sdk (:state result)))
          (is (= 75 (:current-task-id result)))))

      (testing "prints confirmation with task-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (let [output (with-out-str (repl/retry))]
          (is (re-find #"Retrying task 75" output)))))

    (testing "from non-error-recovery state"
      (testing "throws with informative error"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [ex (try
                   (repl/retry)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :invalid-state (:type (ex-data ex))))
          (is (= :selecting-task (:current-state (ex-data ex))))
          (is (= :error-recovery (:required-state (ex-data ex)))))))))

;;; skip Tests

(deftest skip-test
  (testing "skip"
    (testing "from :error-recovery state"
      (testing "transitions to :selecting-task"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/skip)
        (is (= :selecting-task (console/current-state))))

      (testing "preserves current-task-id"
        ;; task-id is preserved in state; it will be overwritten when
        ;; the outer loop selects and starts the next task
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/skip)
        (is (= 75 (:current-task-id @console/console-state))))

      (testing "preserves story-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/skip)
        (is (= 53 (:story-id @console/console-state))))

      (testing "returns new state map"
        (setup-error-recovery-state 53 "sess-1" 75)
        (let [result (repl/skip)]
          (is (map? result))
          (is (= :selecting-task (:state result)))))

      (testing "prints confirmation with task-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (let [output (with-out-str (repl/skip))]
          (is (re-find #"Skipped task 75" output)))))

    (testing "from non-error-recovery state"
      (testing "throws with informative error"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [ex (try
                   (repl/skip)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :invalid-state (:type (ex-data ex))))
          (is (= :selecting-task (:current-state (ex-data ex))))
          (is (= :error-recovery (:required-state (ex-data ex)))))))))

;;; Context Management Tests

(deftest add-context-test
  (testing "add-context"
    (testing "with no active story"
      (testing "throws with informative error"
        (console/reset-state!)
        (let [ex (try
                   (repl/add-context "test context")
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :no-active-story (:type (ex-data ex))))
          (is (= :idle (:current-state (ex-data ex)))))))

    (testing "with active story"
      (testing "calls mcp-tasks with correct arguments"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [captured-args (atom nil)]
          (with-redefs [orchestrator/run-mcp-tasks
                        (fn [& args]
                          (reset! captured-args (vec args))
                          {:success true})]
            (repl/add-context "new context text")
            (is (= ["update"
                    "--task-id" "53"
                    "--shared-context" "new context text"]
                   @captured-args)))))

      (testing "returns CLI result"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (with-redefs [orchestrator/run-mcp-tasks
                      (fn [& _args]
                        {:task {:id 53 :shared-context ["ctx"]}})]
          (let [result (repl/add-context "new context")]
            (is (= {:task {:id 53 :shared-context ["ctx"]}} result)))))

      (testing "prints confirmation with story-id"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (with-redefs [orchestrator/run-mcp-tasks (fn [& _args] {})]
          (let [output (with-out-str (repl/add-context "ctx"))]
            (is (re-find #"Added context to story 53" output))))))))

(deftest view-context-test
  (testing "view-context"
    (testing "with no active story"
      (testing "throws with informative error"
        (console/reset-state!)
        (let [ex (try
                   (repl/view-context)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :no-active-story (:type (ex-data ex))))
          (is (= :idle (:current-state (ex-data ex)))))))

    (testing "with active story"
      (testing "calls mcp-tasks with correct arguments"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [captured-args (atom nil)]
          (with-redefs [orchestrator/run-mcp-tasks
                        (fn [& args]
                          (reset! captured-args (vec args))
                          {:task {:shared-context []}})]
            (repl/view-context)
            (is (= ["show" "--task-id" "53"]
                   @captured-args)))))

      (testing "returns shared-context from CLI result"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (with-redefs [orchestrator/run-mcp-tasks
                      (fn [& _args]
                        {:task {:shared-context ["ctx1" "ctx2"]}})]
          (let [result (repl/view-context)]
            (is (= ["ctx1" "ctx2"] result)))))

      (testing "returns nil when shared-context absent"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (with-redefs [orchestrator/run-mcp-tasks
                      (fn [& _args] {:task {}})]
          (let [result (repl/view-context)]
            (is (nil? result)))))

      (testing "prints numbered context entries"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (with-redefs [orchestrator/run-mcp-tasks
                      (fn [& _args]
                        {:task {:shared-context ["first" "second"]}})]
          (let [output (with-out-str (repl/view-context))]
            (is (re-find #"Shared Context:" output))
            (is (re-find #"1\. first" output))
            (is (re-find #"2\. second" output)))))

      (testing "prints (none) when context empty"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (with-redefs [orchestrator/run-mcp-tasks
                      (fn [& _args] {:task {:shared-context []}})]
          (let [output (with-out-str (repl/view-context))]
            (is (re-find #"\(none\)" output))))))))

;;; Session Tracking Tests

(deftest list-sessions-test
  (testing "list-sessions"
    (testing "with no active story"
      (testing "throws with informative error"
        (console/reset-state!)
        (let [ex (try
                   (repl/list-sessions)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :no-active-story (:type (ex-data ex))))
          (is (= :idle (:current-state (ex-data ex)))))))

    (testing "with active story"
      (testing "returns empty vector when no sessions"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [result (repl/list-sessions)]
          (is (vector? result))
          (is (empty? result))))

      (testing "returns sessions for current story only"
        (console/reset-state!)
        ;; Record sessions for story 42
        (console/transition! :selecting-task {:story-id 42})
        (console/record-session! "sess-other" 70)
        ;; Transition to story 53 via abort + restart
        (console/transition! :error-recovery {:error {:type :test}})
        (console/transition! :idle)
        (console/transition! :selecting-task {:story-id 53})
        (console/record-session! "sess-1" 75)
        (console/record-session! "sess-2" 76)
        (let [result (repl/list-sessions)]
          (is (= 2 (count result))
              "should return only sessions for story 53")
          (is (= "sess-1" (:session-id (first result))))
          (is (= "sess-2" (:session-id (second result))))))

      (testing "prints empty message when no sessions"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [output (with-out-str (repl/list-sessions))]
          (is (re-find #"Sessions for story 53:" output))
          (is (re-find #"\(none\)" output))))

      (testing "prints formatted session list"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/record-session! "sess-abc" 75)
        (let [output (with-out-str (repl/list-sessions))]
          (is (re-find #"Sessions for story 53:" output))
          (is (re-find #"sess-abc" output))
          (is (re-find #"task 75" output)))))))

;;; run-story Tests

(deftest run-story-test
  ;; Verifies run-story orchestrates story execution correctly.
  ;; Tests all outcome paths and human-readable output.
  (testing "run-story"
    (testing "from non-idle state"
      (testing "throws with informative error"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 42})
        (let [ex (try
                   (repl/run-story 53)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :invalid-state (:type (ex-data ex))))
          (is (= :selecting-task (:current-state (ex-data ex))))
          (is (= :idle (:required-state (ex-data ex)))))))

    (testing "from :idle state"
      (testing "with :complete outcome"
        (testing "returns result from orchestrator"
          (console/reset-state!)
          (let [mock-result {:outcome :complete
                             :progress {:completed 5 :total 5}
                             :state {:state :story-complete}}]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id _opts] mock-result)]
              (let [result (repl/run-story 53)]
                (is (= :complete (:outcome result)))
                (is (= {:completed 5 :total 5} (:progress result)))))))

        (testing "prints completion message with task count"
          (console/reset-state!)
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _opts]
                          {:outcome :complete
                           :progress {:completed 5 :total 5}
                           :state {:state :story-complete}})]
            (let [output (with-out-str (repl/run-story 53))]
              (is (re-find #"Story complete!" output))
              (is (re-find #"5 tasks" output))))))

      (testing "with :paused outcome"
        (testing "returns result from orchestrator"
          (console/reset-state!)
          (let [mock-result {:outcome :paused
                             :state {:state :selecting-task}}]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id _opts] mock-result)]
              (let [result (repl/run-story 53)]
                (is (= :paused (:outcome result)))))))

        (testing "prints pause message"
          (console/reset-state!)
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _opts]
                          {:outcome :paused
                           :state {:state :selecting-task}})]
            (let [output (with-out-str (repl/run-story 53))]
              (is (re-find #"Story paused" output))
              (is (re-find #"continue" output))))))

      (testing "with :blocked outcome"
        (testing "returns result from orchestrator"
          (console/reset-state!)
          (let [blocked-tasks [{:id 109
                                :title "Blocked task"
                                :blocking-task-ids [108]}]
                mock-result {:outcome :blocked
                             :blocked-tasks blocked-tasks
                             :progress {:completed 1 :total 2}
                             :state {:state :selecting-task}}]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id _opts] mock-result)]
              (let [result (repl/run-story 53)]
                (is (= :blocked (:outcome result)))
                (is (= blocked-tasks (:blocked-tasks result)))))))

        (testing "prints blocked tasks info"
          (console/reset-state!)
          (let [blocked-tasks [{:id 109
                                :title "Blocked task"
                                :blocking-task-ids [108]}]]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id _opts]
                            {:outcome :blocked
                             :blocked-tasks blocked-tasks
                             :state {:state :selecting-task}})]
              (let [output (with-out-str (repl/run-story 53))]
                (is (re-find #"Story blocked" output))
                (is (re-find #"Task 109" output))
                (is (re-find #"Blocked task" output))
                (is (re-find #"Blocked by:" output)))))))

      (testing "with :no-tasks outcome"
        (testing "returns result from orchestrator"
          (console/reset-state!)
          (let [mock-result {:outcome :no-tasks
                             :state {:state :selecting-task}}]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id _opts] mock-result)]
              (let [result (repl/run-story 53)]
                (is (= :no-tasks (:outcome result)))))))

        (testing "prints no tasks message"
          (console/reset-state!)
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _opts]
                          {:outcome :no-tasks
                           :state {:state :selecting-task}})]
            (let [output (with-out-str (repl/run-story 53))]
              (is (re-find #"no child tasks" output))))))

      (testing "with :error outcome"
        (testing "returns result from orchestrator"
          (console/reset-state!)
          (let [mock-result {:outcome :error
                             :error {:type :exception
                                     :message "Test error"}
                             :state {:state :error-recovery}}]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id _opts] mock-result)]
              (let [result (repl/run-story 53)]
                (is (= :error (:outcome result)))
                (is (= "Test error" (-> result :error :message)))))))

        (testing "prints error message"
          (console/reset-state!)
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _opts]
                          {:outcome :error
                           :error {:type :exception
                                   :message "Test error"}
                           :state {:state :error-recovery}})]
            (let [output (with-out-str (repl/run-story 53))]
              (is (re-find #"execution failed" output))
              (is (re-find #"Test error" output))))))

      (testing "prints 'Running story' message"
        (console/reset-state!)
        (with-redefs [orchestrator/execute-story
                      (fn [_story-id _opts]
                        {:outcome :complete
                         :progress {:completed 1 :total 1}
                         :state {:state :story-complete}})]
          (let [output (with-out-str (repl/run-story 53))]
            (is (re-find #"Running story 53" output)))))

      (testing "passes opts to orchestrator"
        (console/reset-state!)
        (let [captured-opts (atom nil)]
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id opts]
                          (reset! captured-opts opts)
                          {:outcome :complete
                           :progress {:completed 1 :total 1}
                           :state {:state :story-complete}})]
            (repl/run-story 53 {:max-turns 100 :model "claude-sonnet-4"})
            (is (= 100 (:max-turns @captured-opts)))
            (is (= "claude-sonnet-4" (:model @captured-opts))))))

      (testing "passes story-id to orchestrator"
        (console/reset-state!)
        (let [captured-story-id (atom nil)]
          (with-redefs [orchestrator/execute-story
                        (fn [story-id _opts]
                          (reset! captured-story-id story-id)
                          {:outcome :complete
                           :progress {:completed 1 :total 1}
                           :state {:state :story-complete}})]
            (repl/run-story 57)
            (is (= 57 @captured-story-id))))))))
