(ns task-conductor.project.execute-test
  ;; Verify state derivation functions correctly map task/story data to states.
  ;; Tests cover all state derivation paths for both tasks and stories.
  ;; Also tests statechart definitions for task and story execution.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.claude-cli.interface :as claude-cli]
   [task-conductor.project.execute :as execute]
   [task-conductor.statechart-engine.core :as engine]
   [task-conductor.statechart-engine.interface :as sc]))

;;; Task State Derivation Tests

(deftest derive-task-state-test
  (testing "derive-task-state"
    (testing "returns :complete"
      (testing "when status is :closed"
        (is (= :complete
               (execute/derive-task-state {:status :closed}))))

      (testing "when PR is merged"
        (is (= :complete
               (execute/derive-task-state {:status :open
                                           :pr-num 123
                                           :pr-merged? true})))))

    (testing "returns :wait-pr-merge"
      (testing "when PR exists but not merged"
        (is (= :wait-pr-merge
               (execute/derive-task-state {:status :open
                                           :pr-num 123
                                           :meta {:refined "true"}})))))

    (testing "returns :awaiting-pr"
      (testing "when status is :done and code-reviewed is set"
        (is (= :awaiting-pr
               (execute/derive-task-state {:status :done
                                           :code-reviewed "2024-01-15"
                                           :meta {:refined "true"}})))))

    (testing "returns :done"
      (testing "when status is :done without code-reviewed"
        (is (= :done
               (execute/derive-task-state {:status :done
                                           :meta {:refined "true"}}))))

      (testing "when status is :done with nil code-reviewed"
        (is (= :done
               (execute/derive-task-state {:status :done
                                           :code-reviewed nil
                                           :meta {:refined "true"}})))))

    (testing "returns :refined"
      (testing "when refined but no PR"
        (is (= :refined
               (execute/derive-task-state {:status :open
                                           :meta {:refined "true"}}))))

      (testing "when refined with other meta keys"
        (is (= :refined
               (execute/derive-task-state {:status :in-progress
                                           :meta {:refined "2024-01-01"
                                                  :other "data"}})))))

    (testing "returns :unrefined"
      (testing "when meta is nil"
        (is (= :unrefined
               (execute/derive-task-state {:status :open}))))

      (testing "when meta exists but :refined is nil"
        (is (= :unrefined
               (execute/derive-task-state {:status :open
                                           :meta {:other "data"}}))))

      (testing "when task is new with minimal data"
        (is (= :unrefined
               (execute/derive-task-state {})))))))

;;; Story State Derivation Tests

(deftest derive-story-state-test
  (testing "derive-story-state"
    (testing "returns :complete"
      (testing "when status is :closed"
        (is (= :complete
               (execute/derive-story-state {:status :closed} []))))

      (testing "when PR is merged"
        (is (= :complete
               (execute/derive-story-state {:status :open
                                            :pr-num 456
                                            :pr-merged? true}
                                           [{:status :closed}])))))

    (testing "returns :wait-pr-merge"
      (testing "when PR exists but not merged"
        (is (= :wait-pr-merge
               (execute/derive-story-state {:status :open
                                            :pr-num 456
                                            :code-reviewed "2024-01-15"
                                            :meta {:refined "true"}}
                                           [{:status :closed}]))))

      (testing "is not returned when PR exists but children are incomplete"
        (is (= :has-tasks
               (execute/derive-story-state {:status :open
                                            :pr-num 456
                                            :code-reviewed "2024-01-15"
                                            :meta {:refined "true"}}
                                           [{:status :closed}
                                            {:status :open}])))))

    (testing "returns :awaiting-pr"
      (testing "when all children complete and reviewed"
        (is (= :awaiting-pr
               (execute/derive-story-state {:status :open
                                            :code-reviewed "2024-01-15"
                                            :meta {:refined "true"}}
                                           [{:status :closed}
                                            {:status :closed}])))))

    (testing "returns :done"
      (testing "when all children closed but not reviewed"
        (is (= :done
               (execute/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :closed}
                                            {:status :closed}]))))

      (testing "when all children done but not reviewed"
        (is (= :done
               (execute/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :done}
                                            {:status :done}]))))

      (testing "with mix of done and closed children"
        (is (= :done
               (execute/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :done}
                                            {:status :closed}]))))

      (testing "with single completed child"
        (is (= :done
               (execute/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :closed}])))))

    (testing "returns :has-tasks"
      (testing "when incomplete children exist"
        (is (= :has-tasks
               (execute/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :closed}
                                            {:status :open}]))))

      (testing "when all children are open"
        (is (= :has-tasks
               (execute/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :open}
                                            {:status :in-progress}]))))

      (testing "when some children are blocked"
        (is (= :has-tasks
               (execute/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           [{:status :blocked}])))))

    (testing "returns :refined"
      (testing "when refined but no children"
        (is (= :refined
               (execute/derive-story-state {:status :open
                                            :meta {:refined "true"}}
                                           []))))

      (testing "when refined with nil children"
        (is (= :refined
               (execute/derive-story-state {:status :open
                                            :meta {:refined "2024-01-01"}}
                                           nil)))))

    (testing "returns :unrefined"
      (testing "when meta is nil"
        (is (= :unrefined
               (execute/derive-story-state {:status :open} []))))

      (testing "when meta exists but :refined is nil"
        (is (= :unrefined
               (execute/derive-story-state {:status :open
                                            :meta {:other "data"}}
                                           []))))

      (testing "when story is new with minimal data"
        (is (= :unrefined
               (execute/derive-story-state {} [])))))))

;;; Helper Function Tests

(deftest refined?-test
  (testing "refined?"
    (testing "returns truthy when :refined is set in :meta"
      (is (execute/refined? {:meta {:refined "true"}}))
      (is (execute/refined? {:meta {:refined "2024-01-01"}})))

    (testing "returns falsy when :refined is nil or missing"
      (is (not (execute/refined? {})))
      (is (not (execute/refined? {:meta {}})))
      (is (not (execute/refined? {:meta {:other "x"}}))))))

(deftest count-completed-children-test
  ;; Verify count-completed-children counts closed/done children.
  (testing "count-completed-children"
    (testing "returns zero for all open"
      (is (= 0 (execute/count-completed-children
                [{:status :open} {:status :open}]))))

    (testing "counts :closed children"
      (is (= 1 (execute/count-completed-children
                [{:status :closed}]))))

    (testing "counts :done children"
      (is (= 1 (execute/count-completed-children
                [{:status :done}]))))

    (testing "counts string status variants"
      (is (= 2 (execute/count-completed-children
                [{:status "closed"} {:status "done"}]))))

    (testing "excludes :deleted children"
      (is (= 0 (execute/count-completed-children
                [{:status :open} {:status :deleted}]))))

    (testing "counts mixed statuses correctly"
      (is (= 2 (execute/count-completed-children
                [{:status :done} {:status :open} {:status :closed}]))))))

;;; Statechart Definition Tests

(defmacro with-clean-test-env
  "Execute body with a fresh engine state and nullable Claude CLI.
  Resets engine before/after and wraps body in with-nullable-claude-cli."
  [& body]
  `(do
     (engine/reset-engine!)
     (try
       (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
         ~@body)
       (finally
         (engine/reset-engine!)))))

(deftest task-statechart-test
  ;; Verify task-statechart structure and transitions.
  ;; States match derive-task-state values; transitions driven by state events.
  (testing "task-statechart"
    (testing "is a valid statechart definition"
      (is (map? execute/task-statechart))
      (is (= :statechart (:node-type execute/task-statechart))))

    (testing "can be registered with the engine"
      (with-clean-test-env
        (is (= ::task-chart
               (sc/register! ::task-chart execute/task-statechart)))))

    (testing "starts in :idle state"
      (with-clean-test-env
        (sc/register! ::task-chart execute/task-statechart)
        (let [sid (sc/start! ::task-chart)]
          (is (contains? (sc/current-state sid) :idle)))))

    (testing "transitions from :idle to derived state"
      (with-clean-test-env
        (sc/register! ::task-trans execute/task-statechart)
        (let [sid (sc/start! ::task-trans)]
          (sc/send! sid :unrefined)
          (is (contains? (sc/current-state sid) :unrefined)))))

    (testing "follows task execution flow"
      (testing "idle → done"
        (with-clean-test-env
          (sc/register! ::task-flow0 execute/task-statechart)
          (let [sid (sc/start! ::task-flow0)]
            (sc/send! sid :done)
            (is (contains? (sc/current-state sid) :done)))))

      (testing "unrefined → refined"
        (with-clean-test-env
          (sc/register! ::task-flow1 execute/task-statechart)
          (let [sid (sc/start! ::task-flow1)]
            (sc/send! sid :unrefined)
            (sc/send! sid :refined)
            (is (contains? (sc/current-state sid) :refined)))))

      (testing "refined → done"
        (with-clean-test-env
          (sc/register! ::task-flow2 execute/task-statechart)
          (let [sid (sc/start! ::task-flow2)]
            (sc/send! sid :refined)
            (sc/send! sid :done)
            (is (contains? (sc/current-state sid) :done)))))

      (testing "done → awaiting-pr"
        (with-clean-test-env
          (sc/register! ::task-flow2b execute/task-statechart)
          (let [sid (sc/start! ::task-flow2b)]
            (sc/send! sid :done)
            (sc/send! sid :awaiting-pr)
            (is (contains? (sc/current-state sid) :awaiting-pr)))))

      (testing "awaiting-pr → wait-pr-merge"
        (with-clean-test-env
          (sc/register! ::task-flow3 execute/task-statechart)
          (let [sid (sc/start! ::task-flow3)]
            (sc/send! sid :awaiting-pr)
            (sc/send! sid :wait-pr-merge)
            (is (contains? (sc/current-state sid) :wait-pr-merge)))))

      (testing "wait-pr-merge → complete → terminated"
        (with-clean-test-env
          (sc/register! ::task-flow4 execute/task-statechart)
          (let [sid (sc/start! ::task-flow4)]
            (sc/send! sid :wait-pr-merge)
            (sc/send! sid :complete)
            (is (= #{:complete} (sc/current-state sid)))
            (sc/send! sid :terminated)
            (is (= #{} (sc/current-state sid))))))

      (testing "wait-pr-merge → merging-pr on :merge-pr"
        (with-clean-test-env
          (sc/register! ::task-flow5 execute/task-statechart)
          (let [sid (sc/start! ::task-flow5)]
            (sc/send! sid :wait-pr-merge)
            (sc/send! sid :merge-pr)
            (is (contains? (sc/current-state sid) :merging-pr))))))

    (testing "re-derive transitions allow cross-state transitions"
      ;; Re-derive transitions enable on-dev-env-close to move from any
      ;; non-terminal state to any other derived state after manual escalation.
      (testing "unrefined → done"
        (with-clean-test-env
          (sc/register! ::task-rederive1 execute/task-statechart)
          (let [sid (sc/start! ::task-rederive1)]
            (sc/send! sid :unrefined)
            (sc/send! sid :done)
            (is (contains? (sc/current-state sid) :done)))))

      (testing "unrefined → complete"
        (with-clean-test-env
          (sc/register! ::task-rederive2 execute/task-statechart)
          (let [sid (sc/start! ::task-rederive2)]
            (sc/send! sid :unrefined)
            (sc/send! sid :complete)
            (is (= #{:complete} (sc/current-state sid))))))

      (testing "wait-pr-merge → done"
        (with-clean-test-env
          (sc/register! ::task-rederive3 execute/task-statechart)
          (let [sid (sc/start! ::task-rederive3)]
            (sc/send! sid :wait-pr-merge)
            (sc/send! sid :done)
            (is (contains? (sc/current-state sid) :done)))))

      (testing "wait-pr-merge → refined"
        (with-clean-test-env
          (sc/register! ::task-rederive4 execute/task-statechart)
          (let [sid (sc/start! ::task-rederive4)]
            (sc/send! sid :wait-pr-merge)
            (sc/send! sid :refined)
            (is (contains? (sc/current-state sid) :refined))))))

    (testing "transitions to :escalated on :error"
      (testing "enters :escalated with :idle sub-state"
        (with-clean-test-env
          (sc/register! ::task-error execute/task-statechart)
          (let [sid (sc/start! ::task-error)]
            (sc/send! sid :unrefined)
            (sc/send! sid :error)
            (is (= #{:escalated :session-idle} (sc/current-state sid)))))))

    (testing ":escalated sub-state transitions"
      (testing ":on-active transitions to :running"
        (with-clean-test-env
          (sc/register! ::task-sub1 execute/task-statechart)
          (let [sid (sc/start! ::task-sub1)]
            (sc/send! sid :unrefined)
            (sc/send! sid :error)
            (sc/send! sid :on-active)
            (is (= #{:escalated :session-running} (sc/current-state sid))))))

      (testing ":on-session-idle transitions back to :idle"
        (with-clean-test-env
          (sc/register! ::task-sub2 execute/task-statechart)
          (let [sid (sc/start! ::task-sub2)]
            (sc/send! sid :unrefined)
            (sc/send! sid :error)
            (sc/send! sid :on-active)
            (sc/send! sid :on-session-idle)
            (is (= #{:escalated :session-idle} (sc/current-state sid)))))))

    (testing "transitions from :escalated to derived states"
      (testing "escalated → refined"
        (with-clean-test-env
          (sc/register! ::task-esc1 execute/task-statechart)
          (let [sid (sc/start! ::task-esc1)]
            (sc/send! sid :unrefined)
            (sc/send! sid :error)
            (is (= #{:escalated :session-idle} (sc/current-state sid)))
            (sc/send! sid :refined)
            (is (contains? (sc/current-state sid) :refined)))))

      (testing "escalated → done"
        (with-clean-test-env
          (sc/register! ::task-esc2 execute/task-statechart)
          (let [sid (sc/start! ::task-esc2)]
            (sc/send! sid :refined)
            (sc/send! sid :error)
            (sc/send! sid :done)
            (is (contains? (sc/current-state sid) :done)))))

      (testing "escalated → complete"
        (with-clean-test-env
          (sc/register! ::task-esc3 execute/task-statechart)
          (let [sid (sc/start! ::task-esc3)]
            (sc/send! sid :unrefined)
            (sc/send! sid :error)
            (sc/send! sid :complete)
            (is (= #{:complete} (sc/current-state sid))))))

      (testing "from :running sub-state"
        (with-clean-test-env
          (sc/register! ::task-esc4 execute/task-statechart)
          (let [sid (sc/start! ::task-esc4)]
            (sc/send! sid :unrefined)
            (sc/send! sid :error)
            (sc/send! sid :on-active)
            (is (= #{:escalated :session-running} (sc/current-state sid)))
            (sc/send! sid :refined)
            (is (contains? (sc/current-state sid) :refined))))))

    (testing ":terminated is the final state"
      (testing "terminates statechart via complete → terminated"
        (with-clean-test-env
          (sc/register! ::task-final execute/task-statechart)
          (let [sid (sc/start! ::task-final)]
            (sc/send! sid :complete)
            (is (= #{:complete} (sc/current-state sid)))
            (sc/send! sid :terminated)
            (is (= #{} (sc/current-state sid)))))))))

(deftest story-statechart-test
  ;; Verify story-statechart structure and transitions.
  ;; States match derive-story-state values; includes has-tasks loop.
  (testing "story-statechart"
    (testing "is a valid statechart definition"
      (is (map? execute/story-statechart))
      (is (= :statechart (:node-type execute/story-statechart))))

    (testing "can be registered with the engine"
      (with-clean-test-env
        (is (= ::story-chart
               (sc/register! ::story-chart execute/story-statechart)))))

    (testing "starts in :idle state"
      (with-clean-test-env
        (sc/register! ::story-chart execute/story-statechart)
        (let [sid (sc/start! ::story-chart)]
          (is (contains? (sc/current-state sid) :idle)))))

    (testing "transitions from :idle to any derived state"
      (with-clean-test-env
        (sc/register! ::story-idle execute/story-statechart)
        (let [sid (sc/start! ::story-idle)]
          (sc/send! sid :has-tasks)
          (is (contains? (sc/current-state sid) :has-tasks)))))

    (testing "follows story execution flow"
      (testing "unrefined → refined"
        (with-clean-test-env
          (sc/register! ::story-flow1 execute/story-statechart)
          (let [sid (sc/start! ::story-flow1)]
            (sc/send! sid :unrefined)
            (sc/send! sid :refined)
            (is (contains? (sc/current-state sid) :refined)))))

      (testing "refined → has-tasks"
        (with-clean-test-env
          (sc/register! ::story-flow2 execute/story-statechart)
          (let [sid (sc/start! ::story-flow2)]
            (sc/send! sid :refined)
            (sc/send! sid :has-tasks)
            (is (contains? (sc/current-state sid) :has-tasks)))))

      (testing "has-tasks → has-tasks (loop for multiple children)"
        (with-clean-test-env
          (sc/register! ::story-loop execute/story-statechart)
          (let [sid (sc/start! ::story-loop)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :has-tasks)
            (sc/send! sid :has-tasks)
            (is (contains? (sc/current-state sid) :has-tasks)))))

      (testing "has-tasks → done"
        (with-clean-test-env
          (sc/register! ::story-flow3 execute/story-statechart)
          (let [sid (sc/start! ::story-flow3)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :done)
            (is (contains? (sc/current-state sid) :done)))))

      (testing "done → has-tasks (review found issues)"
        (with-clean-test-env
          (sc/register! ::story-rework execute/story-statechart)
          (let [sid (sc/start! ::story-rework)]
            (sc/send! sid :done)
            (sc/send! sid :has-tasks)
            (is (contains? (sc/current-state sid) :has-tasks)))))

      (testing "done → awaiting-pr"
        (with-clean-test-env
          (sc/register! ::story-flow4 execute/story-statechart)
          (let [sid (sc/start! ::story-flow4)]
            (sc/send! sid :done)
            (sc/send! sid :awaiting-pr)
            (is (contains? (sc/current-state sid) :awaiting-pr)))))

      (testing "awaiting-pr → wait-pr-merge"
        (with-clean-test-env
          (sc/register! ::story-flow5 execute/story-statechart)
          (let [sid (sc/start! ::story-flow5)]
            (sc/send! sid :awaiting-pr)
            (sc/send! sid :wait-pr-merge)
            (is (contains? (sc/current-state sid) :wait-pr-merge)))))

      (testing "wait-pr-merge → complete → terminated"
        (with-clean-test-env
          (sc/register! ::story-flow6 execute/story-statechart)
          (let [sid (sc/start! ::story-flow6)]
            (sc/send! sid :wait-pr-merge)
            (sc/send! sid :complete)
            (is (= #{:complete} (sc/current-state sid)))
            (sc/send! sid :terminated)
            (is (= #{} (sc/current-state sid))))))

      (testing "wait-pr-merge → merging-pr on :merge-pr"
        (with-clean-test-env
          (sc/register! ::story-flow7 execute/story-statechart)
          (let [sid (sc/start! ::story-flow7)]
            (sc/send! sid :wait-pr-merge)
            (sc/send! sid :merge-pr)
            (is (contains? (sc/current-state sid) :merging-pr))))))

    (testing "transitions to :escalated on :error from any state"
      (testing "enters :escalated with :idle sub-state"
        (with-clean-test-env
          (sc/register! ::story-error execute/story-statechart)
          (let [sid (sc/start! ::story-error)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :error)
            (is (= #{:escalated :session-idle} (sc/current-state sid)))))))

    (testing ":escalated sub-state transitions"
      (testing ":on-active transitions to :running"
        (with-clean-test-env
          (sc/register! ::story-sub1 execute/story-statechart)
          (let [sid (sc/start! ::story-sub1)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :error)
            (sc/send! sid :on-active)
            (is (= #{:escalated :session-running} (sc/current-state sid))))))

      (testing ":on-session-idle transitions back to :idle"
        (with-clean-test-env
          (sc/register! ::story-sub2 execute/story-statechart)
          (let [sid (sc/start! ::story-sub2)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :error)
            (sc/send! sid :on-active)
            (sc/send! sid :on-session-idle)
            (is (= #{:escalated :session-idle} (sc/current-state sid)))))))

    (testing "transitions from :escalated to derived states"
      (testing "escalated → has-tasks"
        (with-clean-test-env
          (sc/register! ::story-esc1 execute/story-statechart)
          (let [sid (sc/start! ::story-esc1)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :error)
            (is (= #{:escalated :session-idle} (sc/current-state sid)))
            (sc/send! sid :has-tasks)
            (is (contains? (sc/current-state sid) :has-tasks)))))

      (testing "escalated → done"
        (with-clean-test-env
          (sc/register! ::story-esc2 execute/story-statechart)
          (let [sid (sc/start! ::story-esc2)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :error)
            (sc/send! sid :done)
            (is (contains? (sc/current-state sid) :done)))))

      (testing "escalated → complete"
        (with-clean-test-env
          (sc/register! ::story-esc3 execute/story-statechart)
          (let [sid (sc/start! ::story-esc3)]
            (sc/send! sid :unrefined)
            (sc/send! sid :error)
            (sc/send! sid :complete)
            (is (= #{:complete} (sc/current-state sid))))))

      (testing "from :running sub-state"
        (with-clean-test-env
          (sc/register! ::story-esc4 execute/story-statechart)
          (let [sid (sc/start! ::story-esc4)]
            (sc/send! sid :has-tasks)
            (sc/send! sid :error)
            (sc/send! sid :on-active)
            (is (= #{:escalated :session-running} (sc/current-state sid)))
            (sc/send! sid :has-tasks)
            (is (contains? (sc/current-state sid) :has-tasks))))))

    (testing ":terminated is the final state"
      (testing "terminates via complete → terminated"
        (with-clean-test-env
          (sc/register! ::story-final execute/story-statechart)
          (let [sid (sc/start! ::story-final)]
            (sc/send! sid :complete)
            (is (= #{:complete} (sc/current-state sid)))
            (sc/send! sid :terminated)
            (is (= #{} (sc/current-state sid)))))))))

(deftest task-states-test
  ;; Verify task-states matches expected set.
  (testing "task-states"
    (testing "contains all task statechart states"
      (is (= #{:idle :unrefined :refined :done :awaiting-pr
               :wait-pr-merge :merging-pr :complete :terminated
               :escalated :session-idle :session-running}
             execute/task-states)))))

(deftest story-states-test
  ;; Verify story-states matches expected set.
  (testing "story-states"
    (testing "contains all story statechart states"
      (is (= #{:idle :unrefined :refined :has-tasks :done
               :awaiting-pr :wait-pr-merge :merging-pr
               :complete :terminated :escalated
               :session-idle :session-running}
             execute/story-states)))))
