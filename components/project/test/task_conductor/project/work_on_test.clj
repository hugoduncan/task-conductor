(ns task-conductor.project.work-on-test
  ;; Verify state derivation functions correctly map task/story data to states.
  ;; Tests cover all state derivation paths for both tasks and stories.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.project.work-on :as work-on]))

;;; Task State Derivation Tests

(deftest derive-task-state-test
  (testing "derive-task-state"
    (testing "returns :complete"
      (testing "when status is :closed"
        (is (= :complete
               (work-on/derive-task-state {:status :closed}))))

      (testing "when PR is merged"
        (is (= :complete
               (work-on/derive-task-state {:status :open
                                           :pr-num 123
                                           :pr-merged? true})))))

    (testing "returns :wait-pr-merge"
      (testing "when PR exists but not merged"
        (is (= :wait-pr-merge
               (work-on/derive-task-state {:status :open
                                           :pr-num 123
                                           :meta {:refined "true"}})))))

    (testing "returns :refined"
      (testing "when refined but no PR"
        (is (= :refined
               (work-on/derive-task-state {:status :open
                                           :meta {:refined "true"}}))))

      (testing "when refined with other meta keys"
        (is (= :refined
               (work-on/derive-task-state {:status :in-progress
                                           :meta {:refined "2024-01-01"
                                                  :other "data"}})))))

    (testing "returns :unrefined"
      (testing "when meta is nil"
        (is (= :unrefined
               (work-on/derive-task-state {:status :open}))))

      (testing "when meta exists but :refined is nil"
        (is (= :unrefined
               (work-on/derive-task-state {:status :open
                                           :meta {:other "data"}}))))

      (testing "when task is new with minimal data"
        (is (= :unrefined
               (work-on/derive-task-state {})))))))

;;; Story State Derivation Tests

(deftest derive-story-state-test
  (testing "derive-story-state"
    (testing "returns :complete"
      (testing "when status is :closed"
        (is (= :complete
               (work-on/derive-story-state {:status :closed} []))))

      (testing "when PR is merged"
        (is (= :complete
               (work-on/derive-story-state {:status :open
                                            :pr-num 456
                                            :pr-merged? true}
                                           [{:status :closed}])))))

    (testing "returns :wait-pr-merge"
      (testing "when PR exists but not merged"
        (is (= :wait-pr-merge
               (work-on/derive-story-state {:status :open
                                            :pr-num 456
                                            :code-reviewed "2024-01-15"
                                            :meta {:refined "true"}}
                                           [{:status :closed}])))))

    (testing "returns :awaiting-pr"
      (testing "when all children complete and reviewed"
        (is (= :awaiting-pr
               (work-on/derive-story-state {:status :open
                                            :code-reviewed "2024-01-15"
                                            :meta {:refined "true"}}
                                           [{:status :closed}
                                            {:status :closed}])))))

    (testing "returns :awaiting-review"
      (testing "when all children complete but not reviewed"
        (is (= :awaiting-review
               (work-on/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :closed}
                                            {:status :closed}]))))

      (testing "with single completed child"
        (is (= :awaiting-review
               (work-on/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :closed}])))))

    (testing "returns :has-tasks"
      (testing "when incomplete children exist"
        (is (= :has-tasks
               (work-on/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :closed}
                                            {:status :open}]))))

      (testing "when all children are open"
        (is (= :has-tasks
               (work-on/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :open}
                                            {:status :in-progress}]))))

      (testing "when some children are blocked"
        (is (= :has-tasks
               (work-on/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :blocked}])))))

    (testing "returns :refined"
      (testing "when refined but no children"
        (is (= :refined
               (work-on/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           []))))

      (testing "when refined with nil children"
        (is (= :refined
               (work-on/derive-story-state {:status :open
                                            :meta {:refined "2024-01-01"}}
                                           nil)))))

    (testing "returns :unrefined"
      (testing "when meta is nil"
        (is (= :unrefined
               (work-on/derive-story-state {:status :open} []))))

      (testing "when meta exists but :refined is nil"
        (is (= :unrefined
               (work-on/derive-story-state {:status :open
                                            :meta {:other "data"}}
                                           []))))

      (testing "when story is new with minimal data"
        (is (= :unrefined
               (work-on/derive-story-state {} [])))))))

;;; Helper Function Tests

(deftest refined?-test
  (testing "refined?"
    (testing "returns truthy when :refined is set in :meta"
      (is (work-on/refined? {:meta {:refined "true"}}))
      (is (work-on/refined? {:meta {:refined "2024-01-01"}})))

    (testing "returns falsy when :refined is nil or missing"
      (is (not (work-on/refined? {})))
      (is (not (work-on/refined? {:meta {}})))
      (is (not (work-on/refined? {:meta {:other "x"}}))))))
