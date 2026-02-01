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
  "Project directory for integration tests.
   Uses the current working directory, which should be the project root
   containing .mcp-tasks.edn when tests are run."
  (System/getProperty "user.dir"))

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

;;; build-add-args tests
;;
;; Tests that build-add-args correctly translates options maps to CLI argument vectors.
;; Contracts: always starts with ["add" "--category" ... "--title" ...], maps optional fields.

(deftest build-add-args-test
  (testing "build-add-args"
    (testing "with required options only"
      (testing "returns add command with category and title"
        (is (= ["add" "--category" "simple" "--title" "Fix bug"]
               (core/build-add-args {:category "simple" :title "Fix bug"})))))

    (testing "with :description"
      (testing "adds --description flag"
        (is (= ["add" "--category" "medium" "--title" "Add auth"
                "--description" "Implement JWT"]
               (core/build-add-args {:category "medium"
                                     :title "Add auth"
                                     :description "Implement JWT"})))))

    (testing "with :type"
      (testing "adds --type flag with keyword name"
        (is (= ["add" "--category" "large" "--title" "Epic"
                "--type" "story"]
               (core/build-add-args {:category "large"
                                     :title "Epic"
                                     :type :story})))))

    (testing "with :parent-id"
      (testing "adds --parent-id flag with stringified ID"
        (is (= ["add" "--category" "simple" "--title" "Subtask"
                "--parent-id" "42"]
               (core/build-add-args {:category "simple"
                                     :title "Subtask"
                                     :parent-id 42})))))

    (testing "with :prepend"
      (testing "adds --prepend flag"
        (is (= ["add" "--category" "simple" "--title" "Urgent"
                "--prepend"]
               (core/build-add-args {:category "simple"
                                     :title "Urgent"
                                     :prepend true})))))))

;;; build-complete-args tests

(deftest build-complete-args-test
  (testing "build-complete-args"
    (testing "with :task-id"
      (testing "adds --task-id flag"
        (is (= ["complete" "--task-id" "42"]
               (core/build-complete-args {:task-id 42})))))

    (testing "with :title"
      (testing "adds --title flag"
        (is (= ["complete" "--title" "Fix bug"]
               (core/build-complete-args {:title "Fix bug"})))))

    (testing "with :category"
      (testing "adds --category flag"
        (is (= ["complete" "--task-id" "42" "--category" "simple"]
               (core/build-complete-args {:task-id 42 :category "simple"})))))

    (testing "with :comment"
      (testing "adds --comment flag"
        (is (= ["complete" "--task-id" "42" "--comment" "Fixed via PR"]
               (core/build-complete-args {:task-id 42 :comment "Fixed via PR"})))))))

;;; build-update-args tests

(deftest build-update-args-test
  (testing "build-update-args"
    (testing "with :task-id only"
      (testing "returns update command with task-id"
        (is (= ["update" "--task-id" "42"]
               (core/build-update-args {:task-id 42})))))

    (testing "with :title"
      (testing "adds --title flag"
        (is (= ["update" "--task-id" "42" "--title" "New title"]
               (core/build-update-args {:task-id 42 :title "New title"})))))

    (testing "with :status"
      (testing "adds --status flag with keyword name"
        (is (= ["update" "--task-id" "42" "--status" "in-progress"]
               (core/build-update-args {:task-id 42 :status :in-progress})))))

    (testing "with :meta"
      (testing "adds --meta flag with JSON"
        (let [result (core/build-update-args {:task-id 42 :meta {:priority "high"}})]
          (is (= "update" (first result)))
          (is (some #{"--meta"} result))
          (is (some #(= "{\"priority\":\"high\"}" %) result)))))

    (testing "with :relations"
      (testing "adds --relations flag with JSON"
        (let [result (core/build-update-args {:task-id 42
                                              :relations [{:id 1 :relates-to 10 :as-type "blocked-by"}]})]
          (is (some #{"--relations"} result)))))

    (testing "with :shared-context"
      (testing "adds --shared-context flag"
        (is (= ["update" "--task-id" "42" "--shared-context" "Key discovery"]
               (core/build-update-args {:task-id 42 :shared-context "Key discovery"})))))

    (testing "with :pr-num"
      (testing "adds --pr-num flag with stringified value"
        (is (= ["update" "--task-id" "42" "--pr-num" "123"]
               (core/build-update-args {:task-id 42 :pr-num 123})))))))

;;; build-delete-args tests

(deftest build-delete-args-test
  (testing "build-delete-args"
    (testing "with :task-id"
      (testing "adds --task-id flag"
        (is (= ["delete" "--task-id" "42"]
               (core/build-delete-args {:task-id 42})))))

    (testing "with :title-pattern"
      (testing "adds --title-pattern flag"
        (is (= ["delete" "--title-pattern" "old-task"]
               (core/build-delete-args {:title-pattern "old-task"})))))))

;;; build-reopen-args tests

(deftest build-reopen-args-test
  (testing "build-reopen-args"
    (testing "with :task-id"
      (testing "adds --task-id flag"
        (is (= ["reopen" "--task-id" "42"]
               (core/build-reopen-args {:task-id 42})))))

    (testing "with :title-pattern"
      (testing "adds --title-pattern flag"
        (is (= ["reopen" "--title-pattern" "Fix bug"]
               (core/build-reopen-args {:title-pattern "Fix bug"})))))))

;;; Mutation integration tests
;;
;; Tests the full lifecycle of task mutations: add -> complete -> reopen -> delete.
;; Creates a temporary task and cleans up after.

(deftest mutation-lifecycle-integration-test
  ;; Full lifecycle test: add -> update -> complete -> reopen -> delete
  ;; Uses a unique title to avoid conflicts with real tasks.
  (testing "task mutation lifecycle"
    (let [unique-title (str "TEST-" (System/currentTimeMillis))]
      (testing "add-task"
        (testing "creates a new task"
          (let [result (core/add-task {:project-dir project-dir
                                       :category "simple"
                                       :title unique-title
                                       :description "Test task"})]
            (is (not (:error result)) (str "add failed: " (:stderr result)))
            (is (map? (:task result)))
            (is (= unique-title (:title (:task result))))

            (when-let [task-id (:id (:task result))]
              (testing "update-task"
                (testing "modifies task fields"
                  (let [update-result (core/update-task {:project-dir project-dir
                                                         :task-id task-id
                                                         :description "Updated desc"})]
                    (is (not (:error update-result)) (str "update failed: " (:stderr update-result)))
                    (is (map? (:task update-result))))))

              (testing "complete-task"
                (testing "marks task as complete"
                  (let [complete-result (core/complete-task {:project-dir project-dir
                                                             :task-id task-id
                                                             :comment "Done"})]
                    (is (not (:error complete-result)))
                    (is (= "closed" (:status (:task complete-result)))))))

              (testing "reopen-task"
                (testing "reopens a closed task"
                  (let [reopen-result (core/reopen-task {:project-dir project-dir
                                                         :task-id task-id})]
                    (is (not (:error reopen-result)))
                    (is (= "open" (:status (:task reopen-result)))))))

              (testing "delete-task"
                (testing "removes the task"
                  (let [delete-result (core/delete-task {:project-dir project-dir
                                                         :task-id task-id})]
                    (is (not (:error delete-result)))))))))))))

(deftest why-blocked-integration-test
  (testing "why-blocked"
    (testing "with unblocked task"
      (testing "returns blocking info"
        (let [result (core/why-blocked {:project-dir project-dir
                                        :task-id 330})]
          (is (not (:error result)))
          (is (map? result)))))

    (testing "with non-existent task"
      (testing "returns error"
        (let [result (core/why-blocked {:project-dir project-dir
                                        :task-id 999999})]
          (is (= :cli-error (:error result))))))))
