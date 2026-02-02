(ns task-conductor.project.work-on-test
  ;; Verify state derivation functions correctly map task/story data to states.
  ;; Tests cover all state derivation paths for both tasks and stories.
  ;; Also tests statechart definitions for task and story execution.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.project.work-on :as work-on]
   [task-conductor.statechart-engine.core :as engine]
   [task-conductor.statechart-engine.interface :as sc]))

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

    (testing "returns :awaiting-pr"
      (testing "when status is :done and code-reviewed is set"
        (is (= :awaiting-pr
               (work-on/derive-task-state {:status :done
                                           :code-reviewed "2024-01-15"
                                           :meta {:refined "true"}})))))

    (testing "returns :done"
      (testing "when status is :done without code-reviewed"
        (is (= :done
               (work-on/derive-task-state {:status :done
                                           :meta {:refined "true"}}))))

      (testing "when status is :done with nil code-reviewed"
        (is (= :done
               (work-on/derive-task-state {:status :done
                                           :code-reviewed nil
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

;;; Statechart Definition Tests

(defmacro with-clean-engine
  "Execute body with a fresh engine state."
  [& body]
  `(do
     (engine/reset-engine!)
     (try
       ~@body
       (finally
         (engine/reset-engine!)))))

(deftest task-statechart-test
  ;; Verify task-statechart structure and transitions.
  ;; States match derive-task-state values; transitions driven by state events.
  (testing "task-statechart"
    (testing "is a valid statechart definition"
      (is (map? work-on/task-statechart))
      (is (= :statechart (:node-type work-on/task-statechart))))

    (testing "can be registered with the engine"
      (with-clean-engine
        (is (= ::task-chart
               (sc/register! ::task-chart work-on/task-statechart)))))

    (testing "starts in :idle state"
      (with-clean-engine
        (sc/register! ::task-chart work-on/task-statechart)
        (let [sid (sc/start! ::task-chart)]
          (is (contains? (sc/current-state sid) :idle)))))

    (testing "transitions from :idle to derived state"
      (with-clean-engine
        (sc/register! ::task-trans work-on/task-statechart)
        (let [sid (sc/start! ::task-trans)]
          (sc/send! sid :unrefined)
          (is (contains? (sc/current-state sid) :unrefined)))))

    (testing "follows task execution flow"
      (testing "unrefined → refined"
        (with-clean-engine
          (sc/register! ::task-flow1 work-on/task-statechart)
          (let [sid (sc/start! ::task-flow1)]
            (sc/send! sid :unrefined)
            (sc/send! sid :refined)
            (is (contains? (sc/current-state sid) :refined)))))

      (testing "refined → awaiting-pr"
        (with-clean-engine
          (sc/register! ::task-flow2 work-on/task-statechart)
          (let [sid (sc/start! ::task-flow2)]
            (sc/send! sid :refined)
            (sc/send! sid :awaiting-pr)
            (is (contains? (sc/current-state sid) :awaiting-pr)))))

      (testing "awaiting-pr → wait-pr-merge"
        (with-clean-engine
          (sc/register! ::task-flow3 work-on/task-statechart)
          (let [sid (sc/start! ::task-flow3)]
            (sc/send! sid :awaiting-pr)
            (sc/send! sid :wait-pr-merge)
            (is (contains? (sc/current-state sid) :wait-pr-merge)))))

      (testing "wait-pr-merge → complete"
        (with-clean-engine
          (sc/register! ::task-flow4 work-on/task-statechart)
          (let [sid (sc/start! ::task-flow4)]
            (sc/send! sid :wait-pr-merge)
            (sc/send! sid :complete)
            ;; Final state causes termination - configuration becomes empty
            (is (= #{} (sc/current-state sid)))))))

    (testing "transitions to :escalated on :error"
      (with-clean-engine
        (sc/register! ::task-error work-on/task-statechart)
        (let [sid (sc/start! ::task-error)]
          (sc/send! sid :unrefined)
          (sc/send! sid :error)
          (is (contains? (sc/current-state sid) :escalated)))))

    (testing ":complete is a final state"
      (testing "terminates statechart (empty configuration)"
        (with-clean-engine
          (sc/register! ::task-final work-on/task-statechart)
          (let [sid (sc/start! ::task-final)]
            (sc/send! sid :complete)
            ;; Final state causes immediate termination
            (is (= #{} (sc/current-state sid)))))))))

(deftest story-statechart-test
  ;; Verify story-statechart structure and transitions.
  ;; States match derive-story-state values; includes has-tasks loop.
  (testing "story-statechart"
    (testing "is a valid statechart definition"
      (is (map? work-on/story-statechart))
      (is (= :statechart (:node-type work-on/story-statechart))))

    (testing "can be registered with the engine"
      (with-clean-engine
        (is (= ::story-chart
               (sc/register! ::story-chart work-on/story-statechart)))))

    (testing "starts in :idle state"
      (with-clean-engine
        (sc/register! ::story-chart work-on/story-statechart)
        (let [sid (sc/start! ::story-chart)]
          (is (contains? (sc/current-state sid) :idle)))))

    (testing "transitions from :idle to any derived state"
      (with-clean-engine
        (sc/register! ::story-idle work-on/story-statechart)
        (let [sid (sc/start! ::story-idle)]
          (sc/send! sid :has-tasks)
          (is (contains? (sc/current-state sid) :has-tasks)))))

    (testing "follows story execution flow"
      (testing "unrefined → refined"
        (with-clean-engine
          (sc/register! ::story-flow1 work-on/story-statechart)
          (let [sid (sc/start! ::story-flow1)]
            (sc/send! sid :unrefined)
            (sc/send! sid :refined)
            (is (contains? (sc/current-state sid) :refined)))))

      (testing "refined → has-tasks"
        (with-clean-engine
          (sc/register! ::story-flow2 work-on/story-statechart)
          (let [sid (sc/start! ::story-flow2)]
            (sc/send! sid :refined)
            (sc/send! sid :has-tasks)
            (is (contains? (sc/current-state sid) :has-tasks)))))

      (testing "has-tasks → has-tasks (loop for multiple children)"
        (with-clean-engine
          (sc/register! ::story-loop work-on/story-statechart)
          (let [sid (sc/start! ::story-loop)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :has-tasks)
            (sc/send! sid :has-tasks)
            (is (contains? (sc/current-state sid) :has-tasks)))))

      (testing "has-tasks → awaiting-review"
        (with-clean-engine
          (sc/register! ::story-flow3 work-on/story-statechart)
          (let [sid (sc/start! ::story-flow3)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :awaiting-review)
            (is (contains? (sc/current-state sid) :awaiting-review)))))

      (testing "awaiting-review → has-tasks (review found issues)"
        (with-clean-engine
          (sc/register! ::story-rework work-on/story-statechart)
          (let [sid (sc/start! ::story-rework)]
            (sc/send! sid :awaiting-review)
            (sc/send! sid :has-tasks)
            (is (contains? (sc/current-state sid) :has-tasks)))))

      (testing "awaiting-review → awaiting-pr"
        (with-clean-engine
          (sc/register! ::story-flow4 work-on/story-statechart)
          (let [sid (sc/start! ::story-flow4)]
            (sc/send! sid :awaiting-review)
            (sc/send! sid :awaiting-pr)
            (is (contains? (sc/current-state sid) :awaiting-pr)))))

      (testing "awaiting-pr → wait-pr-merge"
        (with-clean-engine
          (sc/register! ::story-flow5 work-on/story-statechart)
          (let [sid (sc/start! ::story-flow5)]
            (sc/send! sid :awaiting-pr)
            (sc/send! sid :wait-pr-merge)
            (is (contains? (sc/current-state sid) :wait-pr-merge)))))

      (testing "wait-pr-merge → complete"
        (with-clean-engine
          (sc/register! ::story-flow6 work-on/story-statechart)
          (let [sid (sc/start! ::story-flow6)]
            (sc/send! sid :wait-pr-merge)
            (sc/send! sid :complete)
            ;; Final state causes termination - configuration becomes empty
            (is (= #{} (sc/current-state sid)))))))

    (testing "transitions to :escalated on :error from any state"
      (with-clean-engine
        (sc/register! ::story-error work-on/story-statechart)
        (let [sid (sc/start! ::story-error)]
          (sc/send! sid :has-tasks)
          (sc/send! sid :error)
          (is (contains? (sc/current-state sid) :escalated)))))

    (testing ":complete is a final state"
      (testing "terminates statechart (empty configuration)"
        (with-clean-engine
          (sc/register! ::story-final work-on/story-statechart)
          (let [sid (sc/start! ::story-final)]
            (sc/send! sid :complete)
            ;; Final state causes immediate termination
            (is (= #{} (sc/current-state sid)))))))))

(deftest task-states-test
  ;; Verify task-states matches expected set.
  (testing "task-states"
    (testing "contains all task statechart states"
      (is (= #{:idle :unrefined :refined :done :awaiting-pr
               :wait-pr-merge :complete :escalated}
             work-on/task-states)))))

(deftest story-states-test
  ;; Verify story-states matches expected set.
  (testing "story-states"
    (testing "contains all story statechart states"
      (is (= #{:idle :unrefined :refined :has-tasks :awaiting-review
               :awaiting-pr :wait-pr-merge :complete :escalated}
             work-on/story-states)))))
