(ns task-conductor.agent-runner.orchestrator-test
  ;; Tests task selection logic for story execution orchestration.
  ;; Uses mocked CLI responses to test edge case handling.
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
