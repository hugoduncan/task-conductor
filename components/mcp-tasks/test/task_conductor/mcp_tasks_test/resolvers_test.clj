(ns task-conductor.mcp-tasks-test.resolvers-test
  ;; Verify resolvers and mutations correctly delegate to interface functions.
  ;; Uses mocked interface functions to avoid actual CLI calls.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.mcp-tasks.interface :as interface]
   [task-conductor.mcp-tasks.resolvers :as resolvers]
   [task-conductor.pathom-graph.interface :as graph]))

(defmacro with-clean-state
  "Execute body with clean graph state."
  [& body]
  `(do
     (graph/reset-graph!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         (graph/reset-graph!)))))

;;; Helper Tests

(deftest prefix-keys-test
  (testing "prefix-keys"
    (testing "adds :task/ namespace to all keys"
      (is (= {:task/id 1 :task/title "Test"}
             (#'resolvers/prefix-keys {:id 1 :title "Test"}))))
    (testing "returns nil for nil input"
      (is (nil? (#'resolvers/prefix-keys nil))))))

(deftest unprefix-keys-test
  (testing "unprefix-keys"
    (testing "removes :task/ namespace from keys"
      (is (= {:id 1 :title "Test"}
             (#'resolvers/unprefix-keys {:task/id 1 :task/title "Test"}))))
    (testing "filters out non-task keys"
      (is (= {:id 1}
             (#'resolvers/unprefix-keys {:task/id 1 :other/key "x"}))))))

;;; Resolver Tests

(deftest task-by-id-test
  (with-clean-state
    (testing "task-by-id"
      (testing "returns task with :task/ prefixed keys"
        (with-redefs [interface/show-task
                      (fn [opts]
                        (is (= "/proj" (:project-dir opts)))
                        (is (= 42 (:task-id opts)))
                        {:task {:id 42 :title "Test" :status "open"}
                         :metadata {}})]
          (let [result (graph/query {:task/id 42 :task/project-dir "/proj"}
                                    [:task/title :task/status])]
            (is (= "Test" (:task/title result)))
            (is (= "open" (:task/status result))))))

      (testing "returns :task/error on CLI error"
        (with-redefs [interface/show-task
                      (fn [_] {:error :cli-error :exit-code 1 :stderr "fail"})]
          (let [result (graph/query {:task/id 999 :task/project-dir "/proj"}
                                    [:task/error])]
            (is (= :cli-error (:error (:task/error result))))))))))

(deftest tasks-list-test
  (with-clean-state
    (testing "tasks-list"
      (testing "returns :task/all with prefixed keys"
        (with-redefs [interface/list-tasks
                      (fn [opts]
                        (is (= "/proj" (:project-dir opts)))
                        {:tasks [{:id 1 :title "A"} {:id 2 :title "B"}]
                         :metadata {:count 2}})]
          (let [result (graph/query {:task/project-dir "/proj"}
                                    [:task/all :task/metadata])]
            (is (= 2 (count (:task/all result))))
            (is (= {:task/id 1 :task/title "A"} (first (:task/all result))))
            (is (= 2 (:count (:task/metadata result)))))))

      (testing "passes filter options"
        (with-redefs [interface/list-tasks
                      (fn [opts]
                        (is (= :open (:status opts)))
                        (is (= 5 (:limit opts)))
                        {:tasks [] :metadata {}})]
          (graph/query {:task/project-dir "/proj"
                        :task/filters {:status :open :limit 5}}
                       [:task/all])))

      (testing "returns :task/list-error on failure"
        (with-redefs [interface/list-tasks
                      (fn [_] {:error :io-error :message "no dir"})]
          (let [result (graph/query {:task/project-dir "/bad"}
                                    [:task/list-error])]
            (is (= :io-error (:error (:task/list-error result))))))))))

(deftest task-blocking-info-test
  (with-clean-state
    (testing "task-blocking-info"
      (testing "returns blocking info with prefixed keys"
        (with-redefs [interface/why-blocked
                      (fn [opts]
                        (is (= 42 (:task-id opts)))
                        {:blocked-by [10 20] :blocking-reason "deps"})]
          (let [result (graph/query {:task/id 42 :task/project-dir "/proj"}
                                    [:task/blocked-by :task/blocking-reason])]
            (is (= [10 20] (:task/blocked-by result)))
            (is (= "deps" (:task/blocking-reason result)))))))))

;;; Mutation Tests

(deftest task-create-test
  (with-clean-state
    (testing "task-create!"
      (testing "creates task with required params"
        (with-redefs [interface/add-task
                      (fn [opts]
                        (is (= "/proj" (:project-dir opts)))
                        (is (= "simple" (:category opts)))
                        (is (= "New task" (:title opts)))
                        {:task {:id 99 :title "New task"}})]
          (let [result (graph/query
                        [`(resolvers/task-create!
                           {:task/project-dir "/proj"
                            :task/category "simple"
                            :task/title "New task"})])]
            (is (= 99 (:id (:task (get-in result [`resolvers/task-create! :task/result]))))))))

      (testing "passes optional params"
        (with-redefs [interface/add-task
                      (fn [opts]
                        (is (= "Desc" (:description opts)))
                        (is (= :bug (:type opts)))
                        (is (= 10 (:parent-id opts)))
                        {:task {:id 100}})]
          (graph/query
           [`(resolvers/task-create!
              {:task/project-dir "/proj"
               :task/category "medium"
               :task/title "Bug"
               :task/description "Desc"
               :task/type :bug
               :task/parent-id 10})]))))))

(deftest task-complete-test
  (with-clean-state
    (testing "task-complete!"
      (testing "completes task by ID"
        (with-redefs [interface/complete-task
                      (fn [opts]
                        (is (= 42 (:task-id opts)))
                        (is (= "/proj" (:project-dir opts)))
                        {:task {:id 42 :status "closed"}})]
          (let [result (graph/query
                        [`(resolvers/task-complete!
                           {:task/project-dir "/proj"
                            :task/id 42})])
                task-result (get-in result [`resolvers/task-complete! :task/result])]
            (is (= "closed" (:status (:task task-result)))))))

      (testing "passes comment"
        (with-redefs [interface/complete-task
                      (fn [opts]
                        (is (= "Done via PR" (:comment opts)))
                        {:task {:id 42}})]
          (graph/query
           [`(resolvers/task-complete!
              {:task/project-dir "/proj"
               :task/id 42
               :task/comment "Done via PR"})]))))))

(deftest task-update-test
  (with-clean-state
    (testing "task-update!"
      (testing "updates task fields"
        (with-redefs [interface/update-task
                      (fn [opts]
                        (is (= 42 (:task-id opts)))
                        (is (= "New title" (:title opts)))
                        (is (= :in-progress (:status opts)))
                        {:task {:id 42 :title "New title"}})]
          (let [result (graph/query
                        [`(resolvers/task-update!
                           {:task/project-dir "/proj"
                            :task/id 42
                            :task/title "New title"
                            :task/status :in-progress})])
                task-result (get-in result [`resolvers/task-update! :task/result])]
            (is (= "New title" (:title (:task task-result))))))))))

(deftest task-delete-test
  (with-clean-state
    (testing "task-delete!"
      (testing "deletes task by ID"
        (with-redefs [interface/delete-task
                      (fn [opts]
                        (is (= 42 (:task-id opts)))
                        {:deleted true})]
          (let [result (graph/query
                        [`(resolvers/task-delete!
                           {:task/project-dir "/proj"
                            :task/id 42})])
                task-result (get-in result [`resolvers/task-delete! :task/result])]
            (is (:deleted task-result)))))

      (testing "deletes by title-pattern"
        (with-redefs [interface/delete-task
                      (fn [opts]
                        (is (= "old-*" (:title-pattern opts)))
                        {:deleted true})]
          (graph/query
           [`(resolvers/task-delete!
              {:task/project-dir "/proj"
               :task/title-pattern "old-*"})]))))))

(deftest task-reopen-test
  (with-clean-state
    (testing "task-reopen!"
      (testing "reopens task by ID"
        (with-redefs [interface/reopen-task
                      (fn [opts]
                        (is (= 42 (:task-id opts)))
                        {:task {:id 42 :status "open"}})]
          (let [result (graph/query
                        [`(resolvers/task-reopen!
                           {:task/project-dir "/proj"
                            :task/id 42})])
                task-result (get-in result [`resolvers/task-reopen! :task/result])]
            (is (= "open" (:status (:task task-result))))))))))
