(ns task-conductor.agent-runner.orchestrator-test
  ;; Tests task selection and execution logic for story orchestration.
  ;; Uses mocked CLI responses to test edge case handling.
  ;; Also tests flow model integration for state-driven execution.
  (:require
   [clojure.java.shell :as shell]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.console :as console]
   [task-conductor.agent-runner.flow :as flow]
   [task-conductor.agent-runner.handoff :as handoff]
   [task-conductor.agent-runner.orchestrator :as orchestrator]))

(deftest select-next-task-test
  ;; Verifies task selection handles all edge cases:
  ;; task available, all blocked, all complete, no tasks.
  (testing "select-next-task"
    (testing "when an unblocked task exists"
      (testing "returns :task-available with the task"
        (let [mock-task {:id 109
                         :title "Test task"
                         :status "open"
                         :is-blocked false}]
          (with-redefs [orchestrator/run-mcp-tasks
                        (fn [& args]
                          (if (some #{"--blocked"} args)
                            {:tasks [mock-task]
                             :metadata {:open-task-count 2
                                        :completed-task-count 1
                                        :returned-count 1
                                        :total-matches 2
                                        :limited? true}}
                            {:tasks []
                             :metadata {:open-task-count 0
                                        :completed-task-count 0}}))]
            (let [result (orchestrator/select-next-task 57)]
              (is (= :task-available (:status result))
                  "should return :task-available status")
              (is (= mock-task (:task result))
                  "should include the task")
              (is (= {:completed 1 :total 3} (:progress result))
                  "should include progress info"))))))

    (testing "when all tasks are blocked"
      (testing "returns :all-blocked with blocker info"
        (let [blocked-tasks [{:id 111
                              :title "Blocked task 1"
                              :status "open"
                              :is-blocked true
                              :blocking-task-ids [109 110]}
                             {:id 112
                              :title "Blocked task 2"
                              :status "open"
                              :is-blocked true
                              :blocking-task-ids [111]}]]
          (with-redefs [orchestrator/run-mcp-tasks
                        (fn [& args]
                          (if (some #{"--blocked"} args)
                            {:tasks []
                             :metadata {:open-task-count 0
                                        :completed-task-count 0
                                        :returned-count 0
                                        :total-matches 0
                                        :limited? false}}
                            {:tasks blocked-tasks
                             :metadata {:open-task-count 2
                                        :completed-task-count 1}}))]
            (let [result (orchestrator/select-next-task 57)]
              (is (= :all-blocked (:status result))
                  "should return :all-blocked status")
              (is (= [{:id 111
                       :title "Blocked task 1"
                       :blocking-task-ids [109 110]}
                      {:id 112
                       :title "Blocked task 2"
                       :blocking-task-ids [111]}]
                     (:blocked-tasks result))
                  "should include blocked task info")
              (is (= {:completed 1 :total 3} (:progress result))
                  "should include progress info"))))))

    (testing "when all tasks are complete"
      (testing "returns :all-complete with progress"
        (with-redefs [orchestrator/run-mcp-tasks
                      (fn [& args]
                        (if (some #{"--blocked"} args)
                          {:tasks []
                           :metadata {:open-task-count 0
                                      :completed-task-count 5
                                      :returned-count 0
                                      :total-matches 0
                                      :limited? false}}
                          {:tasks []
                           :metadata {:open-task-count 0
                                      :completed-task-count 5}}))]
          (let [result (orchestrator/select-next-task 57)]
            (is (= :all-complete (:status result))
                "should return :all-complete status")
            (is (= {:completed 5 :total 5} (:progress result))
                "should include progress info")))))

    (testing "when story has no child tasks"
      (testing "returns :no-tasks"
        (with-redefs [orchestrator/run-mcp-tasks
                      (fn [& _args]
                        {:tasks []
                         :metadata {:open-task-count 0
                                    :completed-task-count 0
                                    :returned-count 0
                                    :total-matches 0
                                    :limited? false}})]
          (let [result (orchestrator/select-next-task 57)]
            (is (= :no-tasks (:status result))
                "should return :no-tasks status")
            (is (nil? (:progress result))
                "should not include progress")))))))

(deftest run-mcp-tasks-test
  ;; Verifies CLI execution and EDN parsing.
  ;; Note: This test requires mcp-tasks CLI to be installed.
  ;; For pure unit testing, we use with-redefs in other tests.
  (testing "run-mcp-tasks"
    (testing "throws on non-zero exit code"
      (with-redefs [shell/sh
                    (fn [& _args]
                      {:exit 1
                       :out ""
                       :err "Command failed"})]
        (let [ex (try
                   (orchestrator/run-mcp-tasks "list")
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex)
              "should throw an exception")
          (is (= 1 (:exit-code (ex-data ex)))
              "should include exit code")
          (is (= "Command failed" (:stderr (ex-data ex)))
              "should include stderr"))))

    (testing "throws on invalid EDN output"
      (with-redefs [shell/sh
                    (fn [& _args]
                      {:exit 0
                       :out "{:unclosed"
                       :err ""})]
        (let [ex (try
                   (orchestrator/run-mcp-tasks "list")
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex)
              "should throw an exception")
          (is (= "{:unclosed" (:output (ex-data ex)))
              "should include output in ex-data"))))

    (testing "parses valid EDN output"
      (with-redefs [shell/sh
                    (fn [& _args]
                      {:exit 0
                       :out "{:tasks [] :metadata {:count 0}}"
                       :err ""})]
        (let [result (orchestrator/run-mcp-tasks "list")]
          (is (= {:tasks [] :metadata {:count 0}} result)
              "should parse EDN output"))))))

(deftest build-task-session-config-test
  ;; Verifies CLI session config construction for task execution.
  ;; Tests :cwd resolution and option merging.
  (testing "build-task-session-config"
    (testing "with minimal task-info"
      (testing "sets :cwd from worktree-path"
        (let [task-info {:worktree-path "/path/to/worktree"
                         :task-id 110}
              result (orchestrator/build-task-session-config task-info)]
          (is (= "/path/to/worktree" (:cwd result))
              "should set :cwd from worktree-path"))))

    (testing "with custom options"
      (testing "merges opts over defaults"
        (let [task-info {:worktree-path "/path/to/worktree"}
              result (orchestrator/build-task-session-config
                      task-info
                      {:timeout-ms 180000})]
          (is (= "/path/to/worktree" (:cwd result))
              "should set :cwd from worktree-path")
          (is (= 180000 (:timeout-ms result))
              "should include custom timeout-ms"))))

    (testing "with :cwd override in opts"
      (testing "uses opts :cwd over task-info worktree-path"
        (let [task-info {:worktree-path "/path/to/worktree"}
              result (orchestrator/build-task-session-config
                      task-info
                      {:cwd "/custom/path"})]
          (is (= "/custom/path" (:cwd result))
              "should use :cwd from opts"))))

    (testing "with nil worktree-path"
      (testing "falls back to current directory"
        (let [task-info {:task-id 110}
              result (orchestrator/build-task-session-config task-info)]
          (is (= (System/getProperty "user.dir") (:cwd result))
              "should fall back to current working directory"))))))

(deftest build-task-prompt-test
  ;; Verifies prompt construction for task execution.
  ;; The prompt should invoke execute-story-child with the parent story ID.
  (testing "build-task-prompt"
    (testing "formats prompt with parent-id"
      (let [task-info {:task-id 111
                       :parent-id 57
                       :worktree-path "/path"}
            result (#'orchestrator/build-task-prompt task-info)]
        (is (= "/mcp-tasks:execute-story-child (MCP) 57" result)
            "should format prompt with parent story ID")))))

(deftest execute-task-test
  ;; Verifies task execution creates CLI session with correct config,
  ;; runs the prompt, and returns session info.
  (testing "execute-task"
    (testing "calls run-cli-session with correct config and prompt"
      (let [captured-config (atom nil)
            captured-prompt (atom nil)
            mock-session-id "mock-session-123"
            mock-messages [{:type :assistant-message :content "Done"}]
            mock-result {:messages mock-messages}]
        (with-redefs [orchestrator/run-cli-session
                      (fn [config prompt]
                        (reset! captured-config config)
                        (reset! captured-prompt prompt)
                        {:result mock-result :session-id mock-session-id})]
          (let [task-info {:task-id 111
                           :parent-id 57
                           :worktree-path "/path/to/worktree"}]
            (orchestrator/execute-task task-info)
            ;; Verify correct prompt was passed
            (is (= "/mcp-tasks:execute-story-child (MCP) 57" @captured-prompt)
                "should pass correct prompt")
            ;; Verify config has expected values
            (is (= "/path/to/worktree" (:cwd @captured-config))
                "should set cwd from worktree-path")))))

    (testing "returns result map with expected keys"
      (let [mock-session-id "session-abc-123"
            mock-messages [{:type :assistant-message
                            :content "Task completed"}]
            mock-result {:messages mock-messages}]
        (with-redefs [orchestrator/run-cli-session
                      (fn [_config _prompt]
                        {:result mock-result :session-id mock-session-id})]
          (let [task-info {:task-id 111
                           :parent-id 57
                           :worktree-path "/path/to/worktree"}
                result (orchestrator/execute-task task-info)]
            (is (= mock-session-id (:session-id result))
                "should include session-id")
            (is (= mock-messages (:messages result))
                "should include messages vector")
            (is (= mock-result (:result result))
                "should include result")
            (is (= false (:handoff-requested? result))
                "should set handoff-requested? to false")))))

    (testing "passes opts to session config"
      (let [captured-config (atom nil)]
        (with-redefs [orchestrator/run-cli-session
                      (fn [config _prompt]
                        (reset! captured-config config)
                        {:result {:messages []} :session-id "sess"})]
          (let [task-info {:task-id 111
                           :parent-id 57
                           :worktree-path "/path/to/worktree"}]
            (orchestrator/execute-task task-info {:timeout-ms 180000})
            (is (= 180000 (:timeout-ms @captured-config))
                "should pass custom opts to session config")))))

    (testing "uses 1-arity form correctly"
      (let [captured-config (atom nil)]
        (with-redefs [orchestrator/run-cli-session
                      (fn [config _prompt]
                        (reset! captured-config config)
                        {:result {:messages []} :session-id "sess"})]
          (let [task-info {:task-id 111
                           :parent-id 57
                           :worktree-path "/path/to/worktree"}]
            (orchestrator/execute-task task-info)
            (is (= "/path/to/worktree" (:cwd @captured-config))
                "should set cwd from worktree-path")))))))

(defn- with-clean-console-state
  "Helper to run body with clean console state and no handoff file I/O."
  [f]
  (console/reset-state!)
  (try
    (with-redefs [handoff/write-handoff-state (fn [& _] nil)]
      (f))
    (finally
      (console/reset-state!))))

(deftest execute-story-test
  ;; Verifies story execution loop handles all outcomes:
  ;; complete, paused, blocked, no-tasks, and error recovery.
  (testing "execute-story"
    (testing "when all tasks complete"
      (testing "returns :complete outcome"
        (with-clean-console-state
          (fn []
            (let [task-counter (atom 0)
                  ;; Tasks from select-next-task don't include :worktree-path
                  tasks [{:id 108 :parent-id 57}
                         {:id 109 :parent-id 57}]]
              (with-redefs [orchestrator/select-next-task
                            (fn [_story-id]
                              (let [n @task-counter]
                                (if (< n (count tasks))
                                  {:status :task-available
                                   :task (nth tasks n)
                                   :progress {:completed n
                                              :total (count tasks)}}
                                  {:status :all-complete
                                   :progress {:completed (count tasks)
                                              :total (count tasks)}})))
                            orchestrator/run-cli-session
                            (fn [_config _prompt]
                              (swap! task-counter inc)
                              {:result {:messages []}
                               :session-id (str "sess-" @task-counter)})]
                (let [result (orchestrator/execute-story 57)]
                  (is (= :complete (:outcome result))
                      "should return :complete")
                  (is (= {:completed 2 :total 2} (:progress result))
                      "should include final progress")
                  (is (= :story-complete (-> result :state :state))
                      "should end in :story-complete state"))))))))

    (testing "when paused during execution"
      (testing "returns :paused outcome"
        (with-clean-console-state
          (fn []
            (let [task {:id 108 :parent-id 57}
                  executed? (atom false)]
              (with-redefs [orchestrator/select-next-task
                            (fn [_story-id]
                              {:status :task-available
                               :task task
                               :progress {:completed 0 :total 1}})
                            orchestrator/run-cli-session
                            (fn [_config _prompt]
                              (reset! executed? true)
                              (console/set-paused!)
                              {:result {:messages []}
                               :session-id "sess-1"})]
                (let [result (orchestrator/execute-story 57)]
                  (is (= :paused (:outcome result))
                      "should return :paused")
                  (is @executed?
                      "should have executed at least one task"))))))))

    (testing "when all tasks are blocked"
      (testing "returns :blocked outcome with blocker info"
        (with-clean-console-state
          (fn []
            (let [blocked-tasks [{:id 109 :title "Blocked" :blocking-task-ids [108]}]]
              (with-redefs [orchestrator/select-next-task
                            (fn [_story-id]
                              {:status :all-blocked
                               :blocked-tasks blocked-tasks
                               :progress {:completed 1 :total 2}})]
                (let [result (orchestrator/execute-story 57)]
                  (is (= :blocked (:outcome result))
                      "should return :blocked")
                  (is (= blocked-tasks (:blocked-tasks result))
                      "should include blocked task info")
                  (is (= {:completed 1 :total 2} (:progress result))
                      "should include progress"))))))))

    (testing "when story has no tasks"
      (testing "returns :no-tasks outcome"
        (with-clean-console-state
          (fn []
            (with-redefs [orchestrator/select-next-task
                          (fn [_story-id]
                            {:status :no-tasks})]
              (let [result (orchestrator/execute-story 57)]
                (is (= :no-tasks (:outcome result))
                    "should return :no-tasks")))))))

    (testing "when an exception occurs during execution"
      (testing "returns :error outcome and transitions to error-recovery"
        (with-clean-console-state
          (fn []
            (with-redefs [orchestrator/select-next-task
                          (fn [_story-id]
                            (throw (ex-info "Test error" {:test-data 42})))]
              (let [result (orchestrator/execute-story 57)]
                (is (= :error (:outcome result))
                    "should return :error")
                (is (= "Test error" (-> result :error :message))
                    "should include error message")
                (is (= {:test-data 42} (-> result :error :data))
                    "should include ex-data")
                (is (= :error-recovery (-> result :state :state))
                    "should transition to :error-recovery")))))))

    (testing "transitions through correct states"
      (testing "goes idle → selecting-task → running-sdk → task-complete → selecting-task"
        (with-clean-console-state
          (fn []
            (let [states (atom [])
                  task {:id 108 :parent-id 57}
                  call-count (atom 0)]
              (with-redefs [orchestrator/select-next-task
                            (fn [_story-id]
                              (swap! states conj (console/current-state))
                              (if (zero? @call-count)
                                (do (swap! call-count inc)
                                    {:status :task-available
                                     :task task
                                     :progress {:completed 0 :total 1}})
                                {:status :all-complete
                                 :progress {:completed 1 :total 1}}))
                            orchestrator/run-cli-session
                            (fn [_config _prompt]
                              (swap! states conj (console/current-state))
                              {:result {:messages []}
                               :session-id "sess-1"})]
                (orchestrator/execute-story 57)
                (is (= :selecting-task (first @states))
                    "should be in :selecting-task when selecting")
                (is (= :running-sdk (second @states))
                    "should be in :running-sdk when executing")))))))))

;;; Flow Model Integration Tests

(defn- mock-story-query-fn
  "Create a mock mcp-tasks function that returns story and children.
   Takes a vector of states to progress through, indexed by call count."
  [states-vec call-counter]
  (fn [_cmd & args]
    (let [args-vec (vec args)
          n (quot @call-counter 2)
          state (get states-vec (min n (dec (count states-vec))))]
      (cond
        (some #{"--unique"} args-vec)
        (do
          (swap! call-counter inc)
          {:tasks [(:story state)]})

        (some #{"--parent-id"} args-vec)
        (do
          (swap! call-counter inc)
          {:tasks (:children state)})

        :else
        {:error "Unexpected query"}))))

(deftest execute-story-with-flow-model-test
  ;; Tests flow model driven story execution through various lifecycle states.
  ;; Contracts tested:
  ;; - Flow model initial-prompt is called first
  ;; - SDK is invoked with flow model provided prompts
  ;; - on-sdk-complete determines next action after SDK turn
  ;; - on-task-complete continues execution between tasks
  ;; - CLI handoff returns appropriate outcome
  ;; - Story completion returns :complete outcome
  ;; - Errors are handled gracefully
  (testing "execute-story-with-flow-model"
    (testing "when story starts in unrefined state"
      (testing "invokes SDK with refine-task prompt"
        (with-clean-console-state
          (fn []
            (let [captured-prompts (atom [])
                  call-counter (atom 0)
                  ;; States: unrefined → refined → with children → complete
                  states [{:story {:id 42 :meta {}} :children []}
                          {:story {:id 42 :meta {:refined true}} :children []}
                          {:story {:id 42 :meta {:refined true}}
                           :children [{:id 101 :status :open}]}
                          {:story {:id 42 :meta {:refined true}}
                           :children [{:id 101 :status :closed}]}
                          {:story {:id 42 :meta {:refined true
                                                 :code-reviewed "ts"}}
                           :children [{:id 101 :status :closed}]}
                          {:story {:id 42 :meta {:refined true
                                                 :code-reviewed "ts"
                                                 :pr-num 99}}
                           :children [{:id 101 :status :closed}]}]
                  mock-fn (mock-story-query-fn states call-counter)
                  fm (flow/story-flow-model mock-fn 42)]
              (with-redefs [orchestrator/run-cli-session
                            (fn [_config prompt]
                              (swap! captured-prompts conj prompt)
                              {:result {:messages []} :session-id "sess-1"})]
                (orchestrator/execute-story-with-flow-model fm 42)
                (is (= "/mcp-tasks:refine-task 42" (first @captured-prompts))
                    "should invoke SDK with refine-task prompt")))))))

    (testing "when story is refined without children"
      (testing "invokes SDK with create-story-children prompt"
        (with-clean-console-state
          (fn []
            (let [captured-prompts (atom [])
                  call-counter (atom 0)
                  ;; States: refined no children → with children → complete
                  states [{:story {:id 42 :meta {:refined true}} :children []}
                          {:story {:id 42 :meta {:refined true}}
                           :children [{:id 101 :status :open}]}
                          {:story {:id 42 :meta {:refined true}}
                           :children [{:id 101 :status :closed}]}
                          {:story {:id 42 :meta {:refined true
                                                 :code-reviewed "ts"}}
                           :children [{:id 101 :status :closed}]}
                          {:story {:id 42 :meta {:refined true
                                                 :code-reviewed "ts"
                                                 :pr-num 99}}
                           :children [{:id 101 :status :closed}]}]
                  mock-fn (mock-story-query-fn states call-counter)
                  fm (flow/story-flow-model mock-fn 42)]
              (with-redefs [orchestrator/run-cli-session
                            (fn [_config prompt]
                              (swap! captured-prompts conj prompt)
                              {:result {:messages []} :session-id "sess-1"})]
                (orchestrator/execute-story-with-flow-model fm 42)
                (is (= "/mcp-tasks:create-story-children 42"
                       (first @captured-prompts))
                    "should invoke SDK with create-story-children")))))))

    (testing "when story has incomplete tasks"
      (testing "invokes SDK with execute-story-child prompt"
        (with-clean-console-state
          (fn []
            (let [captured-prompts (atom [])
                  call-counter (atom 0)
                  ;; States: with open child → closed child → code-reviewed → pr
                  states [{:story {:id 42 :meta {:refined true}}
                           :children [{:id 101 :status :open}]}
                          {:story {:id 42 :meta {:refined true}}
                           :children [{:id 101 :status :closed}]}
                          {:story {:id 42 :meta {:refined true
                                                 :code-reviewed "ts"}}
                           :children [{:id 101 :status :closed}]}
                          {:story {:id 42 :meta {:refined true
                                                 :code-reviewed "ts"
                                                 :pr-num 99}}
                           :children [{:id 101 :status :closed}]}]
                  mock-fn (mock-story-query-fn states call-counter)
                  fm (flow/story-flow-model mock-fn 42)]
              (with-redefs [orchestrator/run-cli-session
                            (fn [_config prompt]
                              (swap! captured-prompts conj prompt)
                              {:result {:messages []} :session-id "sess-1"})]
                (orchestrator/execute-story-with-flow-model fm 42)
                (is (= "/mcp-tasks:execute-story-child 42"
                       (first @captured-prompts))
                    "should invoke SDK with execute-story-child")))))))

    (testing "when all tasks complete and needs code review"
      (testing "invokes SDK with review-story-implementation prompt"
        (with-clean-console-state
          (fn []
            (let [captured-prompts (atom [])
                  call-counter (atom 0)
                  ;; States: all closed no review → code-reviewed → pr
                  states [{:story {:id 42 :meta {:refined true}}
                           :children [{:id 101 :status :closed}]}
                          {:story {:id 42 :meta {:refined true
                                                 :code-reviewed "2025-01"}}
                           :children [{:id 101 :status :closed}]}
                          {:story {:id 42 :meta {:refined true
                                                 :code-reviewed "2025-01"
                                                 :pr-num 99}}
                           :children [{:id 101 :status :closed}]}]
                  mock-fn (mock-story-query-fn states call-counter)
                  fm (flow/story-flow-model mock-fn 42)]
              (with-redefs [orchestrator/run-cli-session
                            (fn [_config prompt]
                              (swap! captured-prompts conj prompt)
                              {:result {:messages []} :session-id "sess-1"})]
                (orchestrator/execute-story-with-flow-model fm 42)
                (is (= "/mcp-tasks:review-story-implementation 42"
                       (first @captured-prompts))
                    "should invoke SDK with review-story-implementation")))))))

    (testing "when code review complete and needs PR"
      (testing "invokes SDK with create-story-pr prompt"
        (with-clean-console-state
          (fn []
            (let [captured-prompts (atom [])
                  call-counter (atom 0)
                  ;; States: code-reviewed needs PR → has PR
                  states [{:story {:id 42 :meta {:refined true
                                                 :code-reviewed "2025-01"}}
                           :children [{:id 101 :status :closed}]}
                          {:story {:id 42 :meta {:refined true
                                                 :code-reviewed "2025-01"
                                                 :pr-num 99}}
                           :children [{:id 101 :status :closed}]}]
                  mock-fn (mock-story-query-fn states call-counter)
                  fm (flow/story-flow-model mock-fn 42)]
              (with-redefs [orchestrator/run-cli-session
                            (fn [_config prompt]
                              (swap! captured-prompts conj prompt)
                              {:result {:messages []} :session-id "sess-1"})
                            console/hand-to-cli
                            (fn []
                              (swap! console/console-state assoc :state :running-cli)
                              {:state @console/console-state})]
                (let [result (orchestrator/execute-story-with-flow-model fm 42)]
                  (is (= "/mcp-tasks:create-story-pr 42"
                         (first @captured-prompts))
                      "should invoke SDK with create-story-pr")
                  (is (= :handed-to-cli (:outcome result))
                      "should return handed-to-cli after PR creation"))))))))

    (testing "when story has PR and awaits manual review"
      (testing "returns handed-to-cli outcome without running SDK"
        (with-clean-console-state
          (fn []
            (let [sdk-called? (atom false)
                  story {:id 42 :meta {:refined true
                                       :code-reviewed "2025-01"
                                       :pr-num 99}}
                  children [{:id 101 :status :closed}]
                  ;; Static state - use vector with single state
                  mock-fn (mock-story-query-fn [{:story story :children children}]
                                               (atom 0))
                  fm (flow/story-flow-model mock-fn 42)]
              (with-redefs [orchestrator/run-cli-session
                            (fn [_config _prompt]
                              (reset! sdk-called? true)
                              {:result {:messages []} :session-id "sess-1"})]
                (let [result (orchestrator/execute-story-with-flow-model fm 42)]
                  (is (not @sdk-called?)
                      "should not call SDK for manual review state")
                  (is (= :handed-to-cli (:outcome result))
                      "should return handed-to-cli outcome")
                  (is (re-find #"manual review" (:reason result))
                      "should include reason about manual review"))))))))

    (testing "when story is complete (PR merged)"
      (testing "returns complete outcome"
        (with-clean-console-state
          (fn []
            (let [story {:id 42 :meta {:refined true
                                       :code-reviewed "2025-01"
                                       :pr-num 99
                                       :pr-merged? true}}
                  children [{:id 101 :status :closed}]
                  mock-fn (mock-story-query-fn [{:story story :children children}]
                                               (atom 0))
                  fm (flow/story-flow-model mock-fn 42)
                  result (orchestrator/execute-story-with-flow-model fm 42)]
              (is (= :complete (:outcome result))
                  "should return complete outcome")
              (is (= :story-complete (-> result :state :state))
                  "should end in story-complete state"))))))

    (testing "when flow model returns error"
      (testing "returns error outcome"
        (with-clean-console-state
          (fn []
            (let [mock-fn (fn [& _] {:error "Connection failed"})
                  fm (flow/story-flow-model mock-fn 42)
                  result (orchestrator/execute-story-with-flow-model fm 42)]
              (is (= :error (:outcome result))
                  "should return error outcome")
              (is (= "Connection failed"
                     (-> result :error :message))
                  "should include error message"))))))

    (testing "when paused during execution"
      (testing "returns paused outcome"
        (with-clean-console-state
          (fn []
            (let [story {:id 42 :meta {:refined true}}
                  children [{:id 101 :status :open}]
                  mock-fn (mock-story-query-fn [{:story story :children children}]
                                               (atom 0))
                  fm (flow/story-flow-model mock-fn 42)]
              (with-redefs [orchestrator/run-cli-session
                            (fn [_config _prompt]
                              (console/set-paused!)
                              {:result {:messages []} :session-id "sess-1"})]
                (let [result (orchestrator/execute-story-with-flow-model fm 42)]
                  (is (= :paused (:outcome result))
                      "should return paused outcome"))))))))))

(deftest execute-story-flow-model-via-opts-test
  ;; Tests passing flow model via opts to execute-story.
  ;; Contracts tested:
  ;; - :flow-model opt triggers flow model execution
  ;; - Flow model is passed through correctly
  (testing "execute-story with :flow-model opt"
    (testing "delegates to flow model execution"
      (with-clean-console-state
        (fn []
          (let [story {:id 42 :meta {:pr-merged? true}}
                children [{:id 101 :status :closed}]
                mock-fn (mock-story-query-fn [{:story story :children children}]
                                             (atom 0))
                fm (flow/story-flow-model mock-fn 42)
                result (orchestrator/execute-story 42 {:flow-model fm})]
            (is (= :complete (:outcome result))
                "should return complete outcome")
            (is (some? (:reason result))
                "should include reason from flow model")))))))

(deftest flow-model-code-review-cycle-test
  ;; Tests the code review cycle: tasks complete → review → CR tasks → execute.
  ;; Contracts tested:
  ;; - After all tasks complete, transitions to code-review state
  ;; - After code review, if CR: tasks created, transitions back to execute-tasks
  ;; - After CR: tasks complete, transitions to create-pr
  (testing "code review cycle"
    (testing "executes CR tasks after code review creates them"
      (with-clean-console-state
        (fn []
          (let [captured-prompts (atom [])
                call-counter (atom 0)
                ;; State progression per query pair (story + children):
                ;; 0. Initial: all tasks complete, need review
                ;; 1. After review SDK: CR task created (back to execute)
                ;; 2. After CR task SDK: all complete, code-reviewed, need PR
                ;; 3. After PR SDK: has pr-num (hands to CLI)
                states [{:story {:id 42 :meta {:refined true}}
                         :children [{:id 101 :status :closed}]}
                        {:story {:id 42 :meta {:refined true}}
                         :children [{:id 101 :status :closed}
                                    {:id 102 :status :open}]}
                        {:story {:id 42 :meta {:refined true
                                               :code-reviewed "ts"}}
                         :children [{:id 101 :status :closed}
                                    {:id 102 :status :closed}]}
                        {:story {:id 42 :meta {:refined true
                                               :code-reviewed "ts"
                                               :pr-num 99}}
                         :children [{:id 101 :status :closed}
                                    {:id 102 :status :closed}]}]
                mock-fn (mock-story-query-fn states call-counter)
                fm (flow/story-flow-model mock-fn 42)]
            (with-redefs [orchestrator/run-cli-session
                          (fn [_config prompt]
                            (swap! captured-prompts conj prompt)
                            {:result {:messages []} :session-id "sess-1"})
                          console/hand-to-cli
                          (fn []
                            (swap! console/console-state assoc :state :running-cli)
                            {:state @console/console-state})]
              (let [result (orchestrator/execute-story-with-flow-model fm 42)]
                ;; Should have called:
                ;; 1. review-story-implementation (all complete, need review)
                ;; 2. execute-story-child (CR task created)
                ;; 3. create-story-pr (all complete again, code-reviewed)
                (is (= "/mcp-tasks:review-story-implementation 42"
                       (first @captured-prompts))
                    "should start with code review")
                (is (= "/mcp-tasks:execute-story-child 42"
                       (second @captured-prompts))
                    "should execute CR task after review")
                (is (= "/mcp-tasks:create-story-pr 42"
                       (nth @captured-prompts 2))
                    "should create PR after CR tasks complete")
                (is (= :handed-to-cli (:outcome result))
                    "should hand to CLI after PR creation")))))))))
