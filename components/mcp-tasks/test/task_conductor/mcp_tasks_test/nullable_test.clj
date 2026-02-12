(ns task-conductor.mcp-tasks-test.nullable-test
  "Tests for mcp-tasks Nullable infrastructure.
  Verifies configurable responses, operation tracking, and query/mutation classification."
  (:require [clojure.test :refer [deftest is testing]]
            [task-conductor.mcp-tasks.interface :as tasks]))

;; Tests that the Nullable provides configurable responses and tracks operations
;; without spawning real CLI processes. Contracts: response progression, operation
;; tracking, query/mutation classification.

(deftest make-nullable-test
  (testing "make-nullable"
    (testing "with no config"
      (testing "creates nullable with empty responses"
        (let [nullable (tasks/make-nullable)]
          (is (= [] @(:responses nullable)))
          (is (= {:queries [] :mutations []} @(:operations nullable))))))

    (testing "with responses config"
      (testing "stores provided responses"
        (let [responses [{:tasks []} {:task {:id 1}}]
              nullable (tasks/make-nullable {:responses responses})]
          (is (= responses @(:responses nullable))))))))

(deftest with-nullable-mcp-tasks-test
  (testing "with-nullable-mcp-tasks"
    (testing "with single response"
      (testing "returns configured response"
        (let [expected {:tasks [{:id 1 :title "Test"}]
                        :metadata {:total 1}}
              nullable (tasks/make-nullable {:responses [expected]})
              result (tasks/with-nullable-mcp-tasks nullable
                       (tasks/list-tasks {:project-dir "/test"}))]
          (is (= expected result)))))

    (testing "with multiple responses"
      (testing "consumes responses in order"
        (let [resp1 {:tasks [{:id 1}] :metadata {}}
              resp2 {:task {:id 1 :status "closed"}}
              resp3 {:task {:id 1 :status "open"}}
              nullable (tasks/make-nullable {:responses [resp1 resp2 resp3]})]
          (tasks/with-nullable-mcp-tasks nullable
            (is (= resp1 (tasks/list-tasks {:project-dir "/p"})))
            (is (= resp2 (tasks/complete-task {:project-dir "/p" :task-id 1})))
            (is (= resp3 (tasks/reopen-task {:project-dir "/p" :task-id 1})))))))

    (testing "when responses exhausted"
      (testing "returns error"
        (let [nullable (tasks/make-nullable {:responses [{:tasks []}]})
              _ (tasks/with-nullable-mcp-tasks nullable
                  (tasks/list-tasks {:project-dir "/p"}))
              result (tasks/with-nullable-mcp-tasks nullable
                       (tasks/list-tasks {:project-dir "/p"}))]
          (is (= :no-more-responses (:error result))))))

    (testing "with error response"
      (testing "returns error map"
        (let [error-resp {:error :cli-error :exit-code 1 :stderr "fail"}
              nullable (tasks/make-nullable {:responses [error-resp]})
              result (tasks/with-nullable-mcp-tasks nullable
                       (tasks/add-task {:project-dir "/p"
                                        :category "simple"
                                        :title "Test"}))]
          (is (= :cli-error (:error result)))
          (is (= 1 (:exit-code result))))))))

(deftest operations-test
  (testing "operations"
    (testing "with no operations"
      (testing "returns empty collections"
        (let [nullable (tasks/make-nullable)]
          (is (= {:queries [] :mutations []} (tasks/operations nullable))))))

    (testing "classifies queries correctly"
      (testing "list-tasks is a query"
        (let [nullable (tasks/make-nullable {:responses [{:tasks []}]})]
          (tasks/with-nullable-mcp-tasks nullable
            (tasks/list-tasks {:project-dir "/p"}))
          (is (= 1 (count (:queries (tasks/operations nullable)))))
          (is (= 0 (count (:mutations (tasks/operations nullable)))))))

      (testing "show-task is a query"
        (let [nullable (tasks/make-nullable {:responses [{:task {:id 1}}]})]
          (tasks/with-nullable-mcp-tasks nullable
            (tasks/show-task {:project-dir "/p" :task-id 1}))
          (is (= 1 (count (:queries (tasks/operations nullable)))))))

      (testing "why-blocked is a query"
        (let [nullable (tasks/make-nullable {:responses [{:blocking []}]})]
          (tasks/with-nullable-mcp-tasks nullable
            (tasks/why-blocked {:project-dir "/p" :task-id 1}))
          (is (= 1 (count (:queries (tasks/operations nullable))))))))

    (testing "classifies mutations correctly"
      (testing "add-task is a mutation"
        (let [nullable (tasks/make-nullable {:responses [{:task {:id 1}}]})]
          (tasks/with-nullable-mcp-tasks nullable
            (tasks/add-task {:project-dir "/p" :category "simple" :title "T"}))
          (is (= 0 (count (:queries (tasks/operations nullable)))))
          (is (= 1 (count (:mutations (tasks/operations nullable)))))))

      (testing "complete-task is a mutation"
        (let [nullable (tasks/make-nullable {:responses [{:task {:id 1}}]})]
          (tasks/with-nullable-mcp-tasks nullable
            (tasks/complete-task {:project-dir "/p" :task-id 1}))
          (is (= 1 (count (:mutations (tasks/operations nullable)))))))

      (testing "update-task is a mutation"
        (let [nullable (tasks/make-nullable {:responses [{:task {:id 1}}]})]
          (tasks/with-nullable-mcp-tasks nullable
            (tasks/update-task {:project-dir "/p" :task-id 1 :title "New"}))
          (is (= 1 (count (:mutations (tasks/operations nullable)))))))

      (testing "delete-task is a mutation"
        (let [nullable (tasks/make-nullable {:responses [{:deleted true}]})]
          (tasks/with-nullable-mcp-tasks nullable
            (tasks/delete-task {:project-dir "/p" :task-id 1}))
          (is (= 1 (count (:mutations (tasks/operations nullable)))))))

      (testing "reopen-task is a mutation"
        (let [nullable (tasks/make-nullable {:responses [{:task {:id 1}}]})]
          (tasks/with-nullable-mcp-tasks nullable
            (tasks/reopen-task {:project-dir "/p" :task-id 1}))
          (is (= 1 (count (:mutations (tasks/operations nullable))))))))

    (testing "records opts and timestamp"
      (testing "captures operation details"
        (let [nullable (tasks/make-nullable {:responses [{:tasks []}]})
              opts {:project-dir "/test/path" :status :open :limit 5}]
          (tasks/with-nullable-mcp-tasks nullable
            (tasks/list-tasks opts))
          (let [op (first (:queries (tasks/operations nullable)))]
            (is (= "/test/path" (get-in op [:opts :project-dir])))
            (is (= ["list" "--status" "open" "--limit" "5"]
                   (get-in op [:opts :args])))
            (is (instance? java.time.Instant (:timestamp op)))))))))

(deftest nullable-isolation-test
  (testing "nullable isolation"
    (testing "different nullables track separately"
      (let [nullable1 (tasks/make-nullable {:responses [{:tasks []}]})
            nullable2 (tasks/make-nullable {:responses [{:tasks []} {:tasks []}]})]
        (tasks/with-nullable-mcp-tasks nullable1
          (tasks/list-tasks {:project-dir "/p1"}))
        (tasks/with-nullable-mcp-tasks nullable2
          (tasks/list-tasks {:project-dir "/p2a"})
          (tasks/list-tasks {:project-dir "/p2b"}))
        (is (= 1 (count (:queries (tasks/operations nullable1)))))
        (is (= 2 (count (:queries (tasks/operations nullable2)))))
        (is (= "/p1" (get-in (first (:queries (tasks/operations nullable1)))
                             [:opts :project-dir])))
        (is (= "/p2a" (get-in (first (:queries (tasks/operations nullable2)))
                              [:opts :project-dir])))))))

(deftest task-progression-test
  (testing "task state progression"
    (testing "simulates task lifecycle"
      (let [;; Task starts open, gets completed, then reopened
            responses [{:tasks [{:id 42 :status "open" :title "Test"}]
                        :metadata {:total 1}}
                       {:task {:id 42 :status "closed"}}
                       {:task {:id 42 :status "open"}}]
            nullable (tasks/make-nullable {:responses responses})]
        (tasks/with-nullable-mcp-tasks nullable
          ;; Query: list open tasks
          (let [r1 (tasks/list-tasks {:project-dir "/p" :status :open})]
            (is (= "open" (get-in r1 [:tasks 0 :status]))))
          ;; Mutation: complete task
          (let [r2 (tasks/complete-task {:project-dir "/p" :task-id 42})]
            (is (= "closed" (get-in r2 [:task :status]))))
          ;; Mutation: reopen task
          (let [r3 (tasks/reopen-task {:project-dir "/p" :task-id 42})]
            (is (= "open" (get-in r3 [:task :status])))))
        ;; Verify operations were tracked
        (let [ops (tasks/operations nullable)]
          (is (= 1 (count (:queries ops))))
          (is (= 2 (count (:mutations ops)))))))))
