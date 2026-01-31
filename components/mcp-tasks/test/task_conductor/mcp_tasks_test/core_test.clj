(ns task-conductor.mcp-tasks-test.core-test
  "Tests for mcp-tasks core CLI wrapper functionality."
  (:require [clojure.test :refer [deftest is testing]]
            [task-conductor.mcp-tasks.core :as core]))

;;; build-list-args tests
;;
;; Tests that build-list-args correctly translates options maps to CLI argument vectors.
;; Contracts: always starts with ["list"], maps each option to correct flag format.

(deftest build-list-args-test
  (testing "build-list-args"
    (testing "with empty options"
      (testing "returns only the list command"
        (is (= ["list"] (core/build-list-args {})))))

    (testing "with :status"
      (testing "adds --status flag with keyword name"
        (is (= ["list" "--status" "open"]
               (core/build-list-args {:status :open}))))
      (testing "handles string status"
        (is (= ["list" "--status" "closed"]
               (core/build-list-args {:status "closed"})))))

    (testing "with :category"
      (testing "adds --category flag"
        (is (= ["list" "--category" "simple"]
               (core/build-list-args {:category "simple"})))))

    (testing "with :type"
      (testing "adds --type flag with keyword name"
        (is (= ["list" "--type" "story"]
               (core/build-list-args {:type :story})))))

    (testing "with :parent-id"
      (testing "adds --parent-id flag with stringified ID"
        (is (= ["list" "--parent-id" "42"]
               (core/build-list-args {:parent-id 42})))))

    (testing "with :task-id"
      (testing "adds --task-id flag with stringified ID"
        (is (= ["list" "--task-id" "99"]
               (core/build-list-args {:task-id 99})))))

    (testing "with :title-pattern"
      (testing "adds --title-pattern flag"
        (is (= ["list" "--title-pattern" "fix.*bug"]
               (core/build-list-args {:title-pattern "fix.*bug"})))))

    (testing "with :blocked"
      (testing "when true adds --blocked true"
        (is (= ["list" "--blocked" "true"]
               (core/build-list-args {:blocked true}))))
      (testing "when false adds --blocked false"
        (is (= ["list" "--blocked" "false"]
               (core/build-list-args {:blocked false}))))
      (testing "when nil is not included"
        (is (= ["list"]
               (core/build-list-args {:blocked nil})))))

    (testing "with :show-blocking"
      (testing "adds --show-blocking flag without value"
        (is (= ["list" "--show-blocking"]
               (core/build-list-args {:show-blocking true}))))
      (testing "when false does not add flag"
        (is (= ["list"]
               (core/build-list-args {:show-blocking false})))))

    (testing "with :limit"
      (testing "adds --limit flag with stringified value"
        (is (= ["list" "--limit" "10"]
               (core/build-list-args {:limit 10})))))

    (testing "with :unique"
      (testing "adds --unique flag without value"
        (is (= ["list" "--unique"]
               (core/build-list-args {:unique true}))))
      (testing "when false does not add flag"
        (is (= ["list"]
               (core/build-list-args {:unique false})))))

    (testing "with multiple options"
      (testing "includes all flags"
        (let [result (core/build-list-args {:status :open
                                            :parent-id 100
                                            :limit 5})]
          (is (= "list" (first result)))
          (is (some #{"--status"} result))
          (is (some #{"--parent-id"} result))
          (is (some #{"--limit"} result)))))))

;;; Integration tests
;;
;; Tests that list-tasks and show-task correctly invoke the CLI and parse results.
;; Uses the current project directory which has a .mcp-tasks.edn file.

(def project-dir
  "/Users/duncan/projects/hugoduncan/task-conductor/330-create-story-component-wrapping")

(deftest list-tasks-integration-test
  (testing "list-tasks"
    (testing "with valid project-dir"
      (testing "returns parsed EDN with :tasks key"
        (let [result (core/list-tasks {:project-dir project-dir
                                       :limit 1})]
          (is (not (:error result)) "should not return error")
          (is (vector? (:tasks result)) ":tasks should be a vector")
          (is (map? (:metadata result)) ":metadata should be a map"))))

    (testing "with :parent-id filter"
      (testing "returns only child tasks"
        (let [result (core/list-tasks {:project-dir project-dir
                                       :parent-id 330
                                       :limit 5})]
          (is (not (:error result)))
          (is (every? #(= 330 (:parent-id %)) (:tasks result))))))

    (testing "with :status filter"
      (testing "returns tasks with matching status"
        (let [result (core/list-tasks {:project-dir project-dir
                                       :status :open
                                       :limit 3})]
          (is (not (:error result)))
          (is (every? #(= "open" (:status %)) (:tasks result))))))

    (testing "with invalid project-dir"
      (testing "returns io-error map"
        (let [result (core/list-tasks {:project-dir "/nonexistent/path"})]
          (is (= :io-error (:error result)))
          (is (string? (:message result))))))))

(deftest show-task-integration-test
  (testing "show-task"
    (testing "with valid task-id"
      (testing "returns parsed EDN with :task key"
        (let [result (core/show-task {:project-dir project-dir
                                      :task-id 330})]
          (is (not (:error result)) "should not return error")
          (is (map? (:task result)) ":task should be a map")
          (is (= 330 (:id (:task result)))))))

    (testing "with non-existent task-id"
      (testing "returns error map"
        (let [result (core/show-task {:project-dir project-dir
                                      :task-id 999999})]
          (is (= :cli-error (:error result))))))))

;;; run-cli tests
;;
;; Tests the low-level CLI execution function.
;; Contracts: returns parsed EDN on success, error map on failure.

(deftest run-cli-test
  (testing "run-cli"
    (testing "with valid command"
      (testing "returns parsed EDN"
        (let [result (core/run-cli {:project-dir project-dir
                                    :args ["list" "--limit" "1"]})]
          (is (not (:error result)))
          (is (map? result)))))

    (testing "with invalid command"
      (testing "returns error map with :cli-error"
        (let [result (core/run-cli {:project-dir project-dir
                                    :args ["invalid-command"]})]
          (is (= :cli-error (:error result)))
          (is (number? (:exit-code result)))
          (is (string? (:stderr result))))))))
