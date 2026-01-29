(ns task-conductor.agent-runner.repl-test
  "Tests for the user-facing REPL control functions.

   Tests verify the public API, state transitions, and error handling
   for interactive story execution control."
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.console :as console]
   [task-conductor.agent-runner.events :as events]
   [task-conductor.agent-runner.flow :as flow]
   [task-conductor.agent-runner.orchestrator :as orchestrator]
   [task-conductor.agent-runner.repl :as repl]
   [task-conductor.workspace.interface :as workspace]))

;;; Test Fixtures

(defmacro with-console-state
  "Execute body with a fresh console state, resetting afterward."
  [& body]
  `(try
     (console/reset-state!)
     ~@body
     (finally
       (console/reset-state!))))

;;; start-story Tests

;; Tests the start-story function which validates console state
;; and transitions from :idle to :selecting-task with a story-id.
;; Contract: Only works from :idle state, throws otherwise.

(deftest start-story-test
  (testing "start-story"
    (testing "when in :idle state"
      (testing "transitions to :selecting-task with story-id"
        (with-console-state
          (let [result (repl/start-story 42)]
            (is (= :selecting-task (:state result)))
            (is (= 42 (:story-id result)))))))

    (testing "when not in :idle state"
      (testing "throws an exception with state info"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 1})
          (let [ex (try
                     (repl/start-story 42)
                     nil
                     (catch Exception e e))]
            (is (some? ex))
            (is (= :invalid-state (:type (ex-data ex))))
            (is (= :selecting-task (:current-state (ex-data ex))))
            (is (= :idle (:required-state (ex-data ex))))))))))

;;; status Tests

;; Tests the status function which returns current console state info.
;; Contract: Returns map with :state, :story-id, :current-task-id, :paused,
;; :executing?, and :outcome (when completed).

(deftest status-test
  (testing "status"
    (testing "returns state info map"
      (with-console-state
        (let [result (repl/status)]
          (is (map? result))
          (is (contains? result :state))
          (is (contains? result :story-id))
          (is (contains? result :current-task-id))
          (is (contains? result :paused))
          (is (contains? result :executing?)))))

    (testing "when story is active"
      (testing "includes story-id in result"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 42})
          (let [result (repl/status)]
            (is (= 42 (:story-id result)))))))

    (testing "when task is running"
      (testing "includes task-id in result"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 42})
          (console/transition! nil :running-sdk {:current-task-id 7 :session-id "test-session"})
          (let [result (repl/status)]
            (is (= 7 (:current-task-id result)))))))

    (testing "execution state"
      (testing "shows :executing? true while execution is in progress"
        (with-console-state
          (let [started (promise)
                proceed (promise)]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id _workspace _opts]
                            (deliver started true)
                            (deref proceed 5000 :timeout)
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story 42)
              (deref started 1000 :timeout)
              (let [result (repl/status)]
                (is (true? (:executing? result))))
              (deliver proceed true)
              (repl/await-completion)))))

      (testing "shows :executing? false and :outcome after completion"
        (with-console-state
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _workspace _opts]
                          {:outcome :complete
                           :progress {:completed 1 :total 1}
                           :state {:state :story-complete}})]
            (repl/run-story 42)
            (repl/await-completion)
            (let [result (repl/status)]
              (is (false? (:executing? result)))
              (is (= :complete (:outcome result))))))))))

;;; pause/continue Tests

;; Tests pause and continue functions which set/clear the pause flag.
;; Contract: pause sets :paused true, continue clears it.

(deftest pause-test
  (testing "pause"
    (testing "sets paused flag to true"
      (with-console-state
        (repl/pause)
        (is (true? (:paused (console/get-workspace-state nil))))))))

(deftest continue-test
  (testing "continue"
    (testing "clears paused flag"
      (with-console-state
        (console/set-paused! nil)
        (repl/continue)
        (is (not (:paused (console/get-workspace-state nil))))))))

;;; abort Tests

;; Tests abort function which cancels execution and returns to :idle.
;; Contract: From any non-idle state, transitions to :idle.
;; Special cases: Already idle (no-op), story-complete (direct to idle).

(deftest abort-test
  (testing "abort"
    (testing "when in :idle state"
      (testing "returns current state without transition"
        (with-console-state
          (let [result (repl/abort)]
            (is (= :idle (:state result)))))))

    (testing "when in :story-complete state"
      (testing "transitions directly to :idle"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 42})
          (console/transition! nil :story-complete)
          (let [result (repl/abort)]
            (is (= :idle (:state result)))))))

    (testing "when in active state"
      (testing "transitions through :error-recovery to :idle"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 42})
          (console/transition! nil :running-sdk {:current-task-id 7 :session-id "test-session"})
          (let [result (repl/abort)]
            (is (= :idle (:state result)))))))

    (testing "when paused"
      (testing "clears pause flag"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 42})
          (console/set-paused! nil)
          (repl/abort)
          (is (not (:paused (console/get-workspace-state nil)))))))))

;;; retry Tests

;; Tests retry function which re-attempts a failed task.
;; Contract: Only works from :error-recovery state, throws otherwise.
;; Transitions back to :running-sdk with the same task-id.

(deftest retry-test
  (testing "retry"
    (testing "when in :error-recovery state"
      (testing "transitions to :running-sdk with same task-id"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 42})
          (console/transition! nil :running-sdk {:current-task-id 7 :session-id "test-session"})
          (console/transition! nil :error-recovery {:error {:type :test-error}})
          (let [result (repl/retry)]
            (is (= :running-sdk (:state result)))
            (is (= 7 (:current-task-id result)))))))

    (testing "when not in :error-recovery state"
      (testing "throws an exception"
        (with-console-state
          (let [ex (try
                     (repl/retry)
                     nil
                     (catch Exception e e))]
            (is (some? ex))
            (is (= :invalid-state (:type (ex-data ex))))
            (is (= :error-recovery (:required-state (ex-data ex))))))))))

;;; skip Tests

;; Tests skip function which skips a failed task.
;; Contract: Only works from :error-recovery state, throws otherwise.
;; Transitions to :selecting-task to pick the next task.

(deftest skip-test
  (testing "skip"
    (testing "when in :error-recovery state"
      (testing "transitions to :selecting-task"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 42})
          (console/transition! nil :running-sdk {:current-task-id 7 :session-id "test-session"})
          (console/transition! nil :error-recovery {:error {:type :test-error}})
          (let [result (repl/skip)]
            (is (= :selecting-task (:state result)))))))

    (testing "when not in :error-recovery state"
      (testing "throws an exception"
        (with-console-state
          (let [ex (try
                     (repl/skip)
                     nil
                     (catch Exception e e))]
            (is (some? ex))
            (is (= :invalid-state (:type (ex-data ex))))))))))

;;; Context Management Tests

;; Tests add-context and view-context functions for story shared-context.
;; Contract: Requires active story, delegates to mcp-tasks CLI.

(deftest add-context-test
  (testing "add-context"
    (testing "when no active story"
      (testing "throws :no-active-story exception"
        (with-console-state
          (let [ex (try
                     (repl/add-context "test context")
                     nil
                     (catch Exception e e))]
            (is (some? ex))
            (is (= :no-active-story (:type (ex-data ex))))))))

    (testing "when story is active"
      (testing "calls mcp-tasks with story-id and context"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 42})
          (let [called-args (atom nil)]
            (with-redefs [orchestrator/run-mcp-tasks
                          (fn [& args]
                            (reset! called-args args)
                            {:success true})]
              (repl/add-context "test context")
              (is (= ["update" "--task-id" "42" "--shared-context" "test context"]
                     @called-args)))))))))

(deftest view-context-test
  (testing "view-context"
    (testing "when no active story"
      (testing "throws :no-active-story exception"
        (with-console-state
          (let [ex (try
                     (repl/view-context)
                     nil
                     (catch Exception e e))]
            (is (some? ex))
            (is (= :no-active-story (:type (ex-data ex))))))))

    (testing "when story is active"
      (testing "returns shared-context from mcp-tasks"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 42})
          (with-redefs [orchestrator/run-mcp-tasks
                        (fn [& _args]
                          {:task {:shared-context ["context 1" "context 2"]}})]
            (let [result (repl/view-context)]
              (is (= ["context 1" "context 2"] result)))))))))

;;; Session Tracking Tests

;; Tests list-sessions function which retrieves sessions for current story.
;; Contract: Requires active story, filters sessions by story-id.

(deftest list-sessions-test
  (testing "list-sessions"
    (testing "when no active story"
      (testing "throws :no-active-story exception"
        (with-console-state
          (let [ex (try
                     (repl/list-sessions)
                     nil
                     (catch Exception e e))]
            (is (some? ex))
            (is (= :no-active-story (:type (ex-data ex))))))))

    (testing "when story is active"
      (testing "returns sessions filtered by story-id"
        (with-console-state
          ;; Record multiple sessions within a single story execution
          ;; (sessions are cleared on transition to :idle)
          (console/transition! nil :selecting-task {:story-id 42})
          (console/record-session! "s1" 1)
          (console/record-session! "s2" 2)
          (console/record-session! "s3" 3)
          (let [result (repl/list-sessions)]
            (is (= 3 (count result)))
            (is (every? #(= 42 (:story-id %)) result))))))))

;;; Workspace-Scoped State Tests

;; Tests that REPL functions properly scope state to workspaces.
;; Verifies that workspace alias/path resolution works correctly
;; and that state is isolated between different workspaces.

(deftest workspace-scoped-state-test
  (testing "workspace-scoped state"
    (testing "start-story"
      (testing "with workspace alias stores state at resolved path"
        (with-console-state
          (workspace/add-project! "/path/to/myproject")
          (repl/start-story :myproject 42)
          (is (= :selecting-task (console/current-state :myproject)))
          (is (= 42 (:story-id (console/get-workspace-state :myproject)))))))

    (testing "status"
      (testing "with workspace path retrieves correct state"
        (with-console-state
          (console/transition! "/specific/path" :selecting-task {:story-id 99})
          (let [result (repl/status "/specific/path")]
            (is (= :selecting-task (:state result)))
            (is (= 99 (:story-id result)))))))

    (testing "pause/continue"
      (testing "with workspace alias affects correct workspace"
        (with-console-state
          (workspace/add-project! "/path/to/project")
          (repl/pause :project)
          (is (true? (:paused (console/get-workspace-state :project))))
          (repl/continue :project)
          (is (not (:paused (console/get-workspace-state :project)))))))

    (testing "abort"
      (testing "with workspace path resets correct workspace"
        (with-console-state
          (console/transition! "/abort/test/path" :selecting-task {:story-id 1})
          (repl/abort "/abort/test/path")
          (is (= :idle (console/current-state "/abort/test/path"))))))

    (testing "retry"
      (testing "with workspace alias retries in correct workspace"
        (with-console-state
          (workspace/add-project! "/retry/project")
          (console/transition! :project :selecting-task {:story-id 42})
          (console/transition! :project :running-sdk {:current-task-id 7 :session-id "test-session"})
          (console/transition! :project :error-recovery {:error {:type :test}})
          (let [result (repl/retry :project)]
            (is (= :running-sdk (:state result)))
            (is (= 7 (:current-task-id result)))))))

    (testing "skip"
      (testing "with workspace path skips in correct workspace"
        (with-console-state
          (console/transition! "/skip/test" :selecting-task {:story-id 42})
          (console/transition! "/skip/test" :running-sdk {:current-task-id 7 :session-id "test-session"})
          (console/transition! "/skip/test" :error-recovery {:error {:type :test}})
          (let [result (repl/skip "/skip/test")]
            (is (= :selecting-task (:state result)))))))

    (testing "add-context"
      (testing "with workspace alias uses correct story-id"
        (with-console-state
          (workspace/add-project! "/context/project")
          (console/transition! :project :selecting-task {:story-id 55})
          (let [called-args (atom nil)]
            (with-redefs [orchestrator/run-mcp-tasks
                          (fn [& args]
                            (reset! called-args args)
                            {:success true})]
              (repl/add-context :project "test")
              (is (= "55" (nth @called-args 2))
                  "should use story-id from workspace-scoped state"))))))

    (testing "view-context"
      (testing "with workspace path retrieves from correct story"
        (with-console-state
          (console/transition! "/view/test" :selecting-task {:story-id 77})
          (let [called-story-id (atom nil)]
            (with-redefs [orchestrator/run-mcp-tasks
                          (fn [& args]
                            (reset! called-story-id (nth args 2))
                            {:task {:shared-context ["test"]}})]
              (repl/view-context "/view/test")
              (is (= "77" @called-story-id)))))))

    (testing "list-sessions"
      (testing "with workspace alias filters sessions correctly"
        (with-console-state
          (workspace/add-project! "/sessions/project")
          ;; Record sessions within a single story execution
          ;; (sessions are scoped to workspace and story)
          (console/transition! :project :selecting-task {:story-id 88})
          (console/record-session! :project "s1" 1 (java.time.Instant/now))
          (console/record-session! :project "s2" 2 (java.time.Instant/now))
          (let [result (repl/list-sessions :project)]
            (is (= 2 (count result)))
            (is (every? #(= 88 (:story-id %)) result))))))))

;;; run-story Tests

;; Tests run-story function which orchestrates full story execution.
;; Contract: Only works from :idle state, delegates to orchestrator.
;; run-story is non-blocking - use await-completion to get the result.

(deftest run-story-test
  (testing "run-story"
    (testing "when not in :idle state"
      (testing "throws an exception"
        (with-console-state
          (console/transition! nil :selecting-task {:story-id 1})
          (let [ex (try
                     (repl/run-story 42)
                     nil
                     (catch Exception e e))]
            (is (some? ex))
            (is (= :invalid-state (:type (ex-data ex))))
            (is (= :idle (:required-state (ex-data ex))))))))

    (testing "when in :idle state"
      (testing "returns immediately with :started status"
        (with-console-state
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _workspace _opts]
                          {:outcome :complete
                           :progress {:completed 1 :total 1}
                           :state {:state :story-complete}})]
            (let [result (repl/run-story 42)]
              (is (= :started (:status result)))
              (is (= 42 (:story-id result)))
              ;; Wait for completion to avoid dangling future
              (repl/await-completion)))))

      (testing "calls orchestrator/execute-story with story-id"
        (with-console-state
          (let [called-story-id (atom nil)]
            (with-redefs [orchestrator/execute-story
                          (fn [story-id _workspace _opts]
                            (reset! called-story-id story-id)
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story 42)
              (repl/await-completion)
              (is (= 42 @called-story-id))))))

      (testing "await-completion returns orchestrator result"
        (with-console-state
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _workspace _opts]
                          {:outcome :complete
                           :progress {:completed 5 :total 5}
                           :state {:state :story-complete}})]
            (repl/run-story 42)
            (let [result (repl/await-completion)]
              (is (= :complete (:outcome result)))
              (is (= 5 (get-in result [:progress :completed])))))))

      (testing "handles :paused outcome"
        (with-console-state
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _workspace _opts]
                          {:outcome :paused
                           :state {:state :selecting-task :paused true}})]
            (repl/run-story 42)
            (let [result (repl/await-completion)]
              (is (= :paused (:outcome result)))))))

      (testing "handles :blocked outcome"
        (with-console-state
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _workspace _opts]
                          {:outcome :blocked
                           :blocked-tasks [{:id 1 :title "Task 1" :blocking-task-ids [2]}]
                           :state {:state :selecting-task}})]
            (repl/run-story 42)
            (let [result (repl/await-completion)]
              (is (= :blocked (:outcome result)))
              (is (seq (:blocked-tasks result)))))))

      (testing "handles :no-tasks outcome"
        (with-console-state
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _workspace _opts]
                          {:outcome :no-tasks
                           :state {:state :idle}})]
            (repl/run-story 42)
            (let [result (repl/await-completion)]
              (is (= :no-tasks (:outcome result)))))))

      (testing "handles :error outcome"
        (with-console-state
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _workspace _opts]
                          {:outcome :error
                           :error {:message "Test error"}
                           :state {:state :error-recovery}})]
            (repl/run-story 42)
            (let [result (repl/await-completion)]
              (is (= :error (:outcome result)))
              (is (= "Test error" (get-in result [:error :message]))))))))

    (testing "arity variants"
      (testing "single arity uses default workspace and empty opts"
        (with-console-state
          (let [captured-args (atom nil)]
            (with-redefs [orchestrator/execute-story
                          (fn [story-id workspace opts]
                            (reset! captured-args {:story-id story-id
                                                   :workspace workspace
                                                   :opts opts})
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story 42)
              (repl/await-completion)
              (is (= 42 (:story-id @captured-args)))
              (is (nil? (:workspace @captured-args)))
              (is (contains? (:opts @captured-args) :flow-model))))))

      (testing "two-arity with map second arg treats as opts"
        (with-console-state
          (let [captured-args (atom nil)]
            (with-redefs [orchestrator/execute-story
                          (fn [story-id workspace opts]
                            (reset! captured-args {:story-id story-id
                                                   :workspace workspace
                                                   :opts opts})
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story 42 {:custom-opt true})
              (repl/await-completion)
              (is (= 42 (:story-id @captured-args)))
              (is (nil? (:workspace @captured-args)))
              (is (true? (get-in @captured-args [:opts :custom-opt])))))))

      (testing "two-arity with non-map second arg treats as workspace"
        (with-console-state
          (let [captured-args (atom nil)]
            (with-redefs [orchestrator/execute-story
                          (fn [story-id workspace opts]
                            (reset! captured-args {:story-id story-id
                                                   :workspace workspace
                                                   :opts opts})
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story "/some/path" 42)
              (repl/await-completion)
              (is (= 42 (:story-id @captured-args)))
              (is (= "/some/path" (:workspace @captured-args)))))))

      (testing "three-arity passes all arguments"
        (with-console-state
          (let [captured-args (atom nil)]
            (with-redefs [orchestrator/execute-story
                          (fn [story-id workspace opts]
                            (reset! captured-args {:story-id story-id
                                                   :workspace workspace
                                                   :opts opts})
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story "/custom/path" 53 {:verbose true})
              (repl/await-completion)
              (is (= 53 (:story-id @captured-args)))
              (is (= "/custom/path" (:workspace @captured-args)))
              (is (true? (get-in @captured-args [:opts :verbose]))))))))

    (testing "flow-model integration"
      (testing "creates StoryFlowModel by default"
        (with-console-state
          (let [captured-opts (atom nil)]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id _workspace opts]
                            (reset! captured-opts opts)
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story 42)
              (repl/await-completion)
              (is (some? (:flow-model @captured-opts)))
              (is (satisfies? flow/FlowModel (:flow-model @captured-opts)))))))

      (testing "allows custom flow-model override"
        (with-console-state
          (let [custom-model (reify flow/FlowModel
                               (on-cli-return [_ _ _] nil)
                               (on-sdk-complete [_ _ _] nil)
                               (on-task-complete [_ _] nil)
                               (initial-prompt [_ _ _] nil))
                captured-opts (atom nil)]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id _workspace opts]
                            (reset! captured-opts opts)
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story 42 {:flow-model custom-model})
              (repl/await-completion)
              (is (= custom-model (:flow-model @captured-opts))))))))

    (testing "error propagation"
      (testing "catches exceptions from orchestrator and returns :error outcome"
        (with-console-state
          (with-redefs [orchestrator/execute-story
                        (fn [_story-id _workspace _opts]
                          (throw (ex-info "Test error" {:type :test})))]
            (repl/run-story 42)
            (let [result (repl/await-completion)]
              (is (= :error (:outcome result)))
              (is (= "Test error" (get-in result [:error :message]))))))))

    (testing "workspace integration"
      (testing "with keyword alias passes keyword to orchestrator"
        ;; Workspace resolution happens in console functions, not at REPL entry point
        (with-console-state
          (workspace/add-project! "/path/to/myproject")
          (let [captured-workspace (atom nil)]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id workspace _opts]
                            (reset! captured-workspace workspace)
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story :myproject 53)
              (repl/await-completion :myproject)
              (is (= :myproject @captured-workspace)
                  "should pass keyword alias through to orchestrator")))))

      (testing "with nil workspace passes nil to orchestrator"
        (with-console-state
          (let [captured-workspace (atom :not-set)]
            (with-redefs [orchestrator/execute-story
                          (fn [_story-id workspace _opts]
                            (reset! captured-workspace workspace)
                            {:outcome :complete
                             :progress {:completed 1 :total 1}
                             :state {:state :story-complete}})]
              (repl/run-story nil 53 {})
              (repl/await-completion)
              (is (nil? @captured-workspace)
                  "should pass nil workspace to orchestrator"))))))))

;;; Events Query API Tests

;; Tests the events function which retrieves events from the buffer.
;; Contract: Returns events filtered by session-id, filter map, or current session.
;; Uses console state to determine current session when called with no args.

(defmacro with-events-buffer
  "Execute body with a cleared events buffer, resetting afterward."
  [& body]
  `(try
     (events/clear-events!)
     ~@body
     (finally
       (events/clear-events!))))

(deftest events-test
  (testing "events"
    (testing "when called with no args and no active session"
      (testing "returns empty vector with message"
        (with-console-state
          (with-events-buffer
            (let [result (repl/events)]
              (is (= [] result)))))))

    (testing "when called with no args and active session"
      (testing "returns events for current session"
        (with-console-state
          (with-events-buffer
            (console/transition! nil :selecting-task {:story-id 42})
            (console/transition! nil :running-sdk {:current-task-id 1
                                                   :session-id "active-session"})
            (events/add-event! {:timestamp (java.time.Instant/now)
                                :session-id "active-session"
                                :story-id 42
                                :type :text-block
                                :content {:text "hello"}})
            (events/add-event! {:timestamp (java.time.Instant/now)
                                :session-id "other-session"
                                :story-id 42
                                :type :text-block
                                :content {:text "other"}})
            (let [result (repl/events)]
              (is (= 1 (count result)))
              (is (= "active-session" (:session-id (first result)))))))))

    (testing "when called with session-id string"
      (testing "returns events for that session"
        (with-console-state
          (with-events-buffer
            (events/add-event! {:timestamp (java.time.Instant/now)
                                :session-id "session-a"
                                :story-id 1
                                :type :text-block})
            (events/add-event! {:timestamp (java.time.Instant/now)
                                :session-id "session-b"
                                :story-id 2
                                :type :tool-use-block})
            (let [result (repl/events "session-a")]
              (is (= 1 (count result)))
              (is (= "session-a" (:session-id (first result)))))))))

    (testing "when called with filter map"
      (testing "returns events matching the filter"
        (with-console-state
          (with-events-buffer
            (events/add-event! {:timestamp (java.time.Instant/now)
                                :session-id "s1"
                                :story-id 1
                                :type :text-block})
            (events/add-event! {:timestamp (java.time.Instant/now)
                                :session-id "s1"
                                :story-id 1
                                :type :tool-use-block})
            (events/add-event! {:timestamp (java.time.Instant/now)
                                :session-id "s1"
                                :story-id 1
                                :type :text-block})
            (let [result (repl/events {:type :text-block})]
              (is (= 2 (count result)))
              (is (every? #(= :text-block (:type %)) result)))))))

    (testing "when called with invalid argument"
      (testing "throws an exception"
        (with-console-state
          (with-events-buffer
            (is (thrown-with-msg?
                 clojure.lang.ExceptionInfo
                 #"Invalid argument"
                 (repl/events 123)))))))

    (testing "workspace integration"
      (testing "uses session from specified workspace"
        (with-console-state
          (with-events-buffer
            (workspace/add-project! "/path/to/proj")
            (console/transition! :proj :selecting-task {:story-id 99})
            (console/transition! :proj :running-sdk {:current-task-id 1
                                                     :session-id "ws-session"})
            (events/add-event! {:timestamp (java.time.Instant/now)
                                :session-id "ws-session"
                                :story-id 99
                                :type :text-block})
            (let [result (repl/events :proj nil)]
              (is (= 1 (count result)))
              (is (= "ws-session" (:session-id (first result)))))))))))

;;; event-stats Tests

;; Tests the event-stats function which provides summary statistics.
;; Contract: Returns counts by type, session, and story.

(deftest event-stats-test
  (testing "event-stats"
    (testing "returns zero counts for empty buffer"
      (with-events-buffer
        (let [result (repl/event-stats)]
          (is (= 0 (:total result)))
          (is (= {} (:by-type result)))
          (is (= {} (:by-session result)))
          (is (= {} (:by-story result))))))

    (testing "returns correct counts for populated buffer"
      (with-events-buffer
        (events/add-event! {:timestamp (java.time.Instant/now)
                            :session-id "s1"
                            :story-id 1
                            :type :text-block})
        (events/add-event! {:timestamp (java.time.Instant/now)
                            :session-id "s1"
                            :story-id 1
                            :type :text-block})
        (events/add-event! {:timestamp (java.time.Instant/now)
                            :session-id "s1"
                            :story-id 2
                            :type :tool-use-block})
        (events/add-event! {:timestamp (java.time.Instant/now)
                            :session-id "s2"
                            :story-id 2
                            :type :result-message})
        (let [result (repl/event-stats)]
          (is (= 4 (:total result)))
          (is (= {:text-block 2 :tool-use-block 1 :result-message 1}
                 (:by-type result)))
          (is (= {"s1" 3 "s2" 1} (:by-session result)))
          (is (= {1 2 2 2} (:by-story result))))))

    (testing "respects filter parameter"
      (with-events-buffer
        (events/add-event! {:timestamp (java.time.Instant/now)
                            :session-id "s1"
                            :story-id 1
                            :type :text-block})
        (events/add-event! {:timestamp (java.time.Instant/now)
                            :session-id "s2"
                            :story-id 1
                            :type :text-block})
        (let [result (repl/event-stats {:session-id "s1"})]
          (is (= 1 (:total result)))
          (is (= {"s1" 1} (:by-session result))))))))
