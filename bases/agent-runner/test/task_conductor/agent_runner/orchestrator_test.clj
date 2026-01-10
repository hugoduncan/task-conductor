(ns task-conductor.agent-runner.orchestrator-test
  ;; Tests task selection and execution logic for story orchestration.
  ;; Uses mocked CLI and SDK responses to test edge case handling.
  (:require
   [clojure.java.shell :as shell]
   [clojure.test :refer [deftest is testing]]
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
  ;; Verifies SDK session config construction for task execution.
  ;; Tests MCP auto-discovery vs explicit config, and option merging.
  (testing "build-task-session-config"
    (testing "with minimal task-info"
      (testing "uses auto-discovery and default permission mode"
        (let [task-info {:worktree-path "/path/to/worktree"
                         :task-id 110}
              result (orchestrator/build-task-session-config task-info)]
          (is (= "/path/to/worktree" (:cwd result))
              "should set :cwd from worktree-path")
          (is (= "bypassPermissions" (:permission-mode result))
              "should default to bypassPermissions")
          (is (= ["project"] (:setting-sources result))
              "should enable project auto-discovery"))))

    (testing "with explicit :mcp-servers option"
      (testing "uses explicit config instead of auto-discovery"
        (let [task-info {:worktree-path "/path/to/worktree"}
              mcp-config {"mcp-tasks" {:command "mcp-tasks"
                                       :args ["serve"]}}
              result (orchestrator/build-task-session-config
                      task-info
                      {:mcp-servers mcp-config})]
          (is (= mcp-config (:mcp-servers result))
              "should include explicit mcp-servers")
          (is (nil? (:setting-sources result))
              "should not include setting-sources"))))

    (testing "with custom options"
      (testing "merges opts over defaults"
        (let [task-info {:worktree-path "/path/to/worktree"}
              result (orchestrator/build-task-session-config
                      task-info
                      {:max-turns 50
                       :model "claude-sonnet-4"})]
          (is (= 50 (:max-turns result))
              "should include custom max-turns")
          (is (= "claude-sonnet-4" (:model result))
              "should include custom model")
          (is (= "bypassPermissions" (:permission-mode result))
              "should preserve default permission-mode"))))

    (testing "with :cwd override in opts"
      (testing "uses opts :cwd over task-info worktree-path"
        (let [task-info {:worktree-path "/path/to/worktree"}
              result (orchestrator/build-task-session-config
                      task-info
                      {:cwd "/custom/path"})]
          (is (= "/custom/path" (:cwd result))
              "should use :cwd from opts"))))

    (testing "with permission-mode override"
      (testing "allows overriding default permission-mode"
        (let [task-info {:worktree-path "/path/to/worktree"}
              result (orchestrator/build-task-session-config
                      task-info
                      {:permission-mode "default"})]
          (is (= "default" (:permission-mode result))
              "should use overridden permission-mode"))))))

(deftest build-task-prompt-test
  ;; Verifies prompt construction for task execution.
  ;; The prompt should invoke execute-story-child with the parent story ID.
  (testing "build-task-prompt"
    (testing "formats prompt with parent-id"
      (let [task-info {:task-id 111
                       :parent-id 57
                       :worktree-path "/path"}
            result (#'orchestrator/build-task-prompt task-info)]
        (is (= "/mcp-tasks:execute-story-child 57" result)
            "should format prompt with parent story ID")))))

(deftest execute-task-test
  ;; Verifies task execution creates SDK session with correct config,
  ;; runs the prompt, and returns session info.
  (testing "execute-task"
    (testing "calls run-sdk-session with correct config and prompt"
      (let [captured-config (atom nil)
            captured-prompt (atom nil)
            mock-session-id "mock-session-123"
            mock-messages [{:type :assistant-message :content "Done"}]
            mock-result {:messages mock-messages}]
        (with-redefs [orchestrator/run-sdk-session
                      (fn [config prompt]
                        (reset! captured-config config)
                        (reset! captured-prompt prompt)
                        {:result mock-result :session-id mock-session-id})]
          (let [task-info {:task-id 111
                           :parent-id 57
                           :worktree-path "/path/to/worktree"}]
            (orchestrator/execute-task task-info)
            ;; Verify correct prompt was passed
            (is (= "/mcp-tasks:execute-story-child 57" @captured-prompt)
                "should pass correct prompt")
            ;; Verify config has expected values
            (is (= "/path/to/worktree" (:cwd @captured-config))
                "should set cwd from worktree-path")
            (is (= "bypassPermissions" (:permission-mode @captured-config))
                "should use bypassPermissions")))))

    (testing "returns result map with expected keys"
      (let [mock-session-id "session-abc-123"
            mock-messages [{:type :assistant-message
                            :content "Task completed"}]
            mock-result {:messages mock-messages}]
        (with-redefs [orchestrator/run-sdk-session
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
        (with-redefs [orchestrator/run-sdk-session
                      (fn [config _prompt]
                        (reset! captured-config config)
                        {:result {:messages []} :session-id "sess"})]
          (let [task-info {:task-id 111
                           :parent-id 57
                           :worktree-path "/path/to/worktree"}]
            (orchestrator/execute-task task-info {:max-turns 100})
            (is (= 100 (:max-turns @captured-config))
                "should pass custom opts to session config")))))

    (testing "uses 1-arity form correctly"
      (let [captured-config (atom nil)]
        (with-redefs [orchestrator/run-sdk-session
                      (fn [config _prompt]
                        (reset! captured-config config)
                        {:result {:messages []} :session-id "sess"})]
          (let [task-info {:task-id 111
                           :parent-id 57
                           :worktree-path "/path/to/worktree"}]
            (orchestrator/execute-task task-info)
            (is (= ["project"] (:setting-sources @captured-config))
                "should use auto-discovery by default")))))))
