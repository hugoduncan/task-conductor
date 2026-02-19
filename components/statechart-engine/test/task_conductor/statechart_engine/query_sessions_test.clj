(ns task-conductor.statechart-engine.query-sessions-test
  ;; Tests for query-sessions: filtering active sessions by state.
  ;; Verifies correct filtering, data extraction, and edge cases
  ;; using a multi-state chart with escalated and idle states.
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.fulcrologic.statecharts.elements :refer [state transition]]
   [com.fulcrologic.statecharts.chart :refer [statechart]]
   [task-conductor.statechart-engine.core :as core]
   [task-conductor.statechart-engine.test-helpers :refer [with-clean-engine]]))

(def test-chart
  "Chart with idle, running, and escalated states."
  (statechart {:initial :idle}
              (state {:id :idle}
                     (transition {:event :start :target :running}))
              (state {:id :running}
                     (transition {:event :error :target :escalated})
                     (transition {:event :done :target :idle}))
              (state {:id :escalated}
                     (transition {:event :resolve :target :idle}))))

(deftest query-sessions-test
  (testing "query-sessions"
    (testing "returns empty vec when no sessions exist"
      (with-clean-engine
        (is (= [] (core/query-sessions #{:escalated :idle})))))

    (testing "returns empty vec when no sessions match filter"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [sid (core/start! ::chart
                               {:data {:task-id 1
                                       :task-title "Task A"}})]
          ;; Session starts in :idle, filter for :escalated only
          (core/send! sid :start)
          (is (= [] (core/query-sessions #{:escalated}))))))

    (testing "returns sessions matching filter"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [sid (core/start! ::chart
                               {:data {:task-id 10
                                       :task-title "Esc task"}})]
          (core/send! sid :start)
          (core/send! sid :error)
          (let [result (core/query-sessions #{:escalated})]
            (is (= 1 (count result)))
            (is (= sid (:session-id (first result))))
            (is (= :escalated (:state (first result))))
            (is (= 10 (:task-id (first result))))
            (is (= "Esc task" (:task-title (first result))))
            (is (inst? (:entered-state-at (first result))))))))

    (testing "filters idle sessions"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (core/start! ::chart {:data {:task-id 5
                                     :task-title "Idle one"}})
        (let [result (core/query-sessions #{:idle})]
          (is (= 1 (count result)))
          (is (= :idle (:state (first result))))
          (is (= 5 (:task-id (first result)))))))

    (testing "matches multiple filter states"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [s1 (core/start! ::chart
                              {:data {:task-id 1
                                      :task-title "Idle"}})
              s2 (core/start! ::chart
                              {:data {:task-id 2
                                      :task-title "Escalated"}})]
          (core/send! s2 :start)
          (core/send! s2 :error)
          (let [result (core/query-sessions #{:idle :escalated})
                by-id (into {} (map (juxt :session-id identity)) result)]
            (is (= 2 (count result)))
            (is (= :idle (:state (get by-id s1))))
            (is (= :escalated (:state (get by-id s2))))))))

    (testing "excludes sessions not matching filter"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [_s1 (core/start! ::chart
                               {:data {:task-id 1}})
              s2 (core/start! ::chart
                              {:data {:task-id 2}})]
          ;; s1 stays in :idle, s2 goes to :running
          (core/send! s2 :start)
          (let [result (core/query-sessions #{:idle})]
            (is (= 1 (count result)))
            (is (= 1 (:task-id (first result))))))))

    (testing "returns nil for missing task-title"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (core/start! ::chart {:data {:task-id 99}})
        (let [result (core/query-sessions #{:idle})]
          (is (= 1 (count result)))
          (is (= 99 (:task-id (first result))))
          (is (nil? (:task-title (first result)))))))))
