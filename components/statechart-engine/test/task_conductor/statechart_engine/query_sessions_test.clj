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
  "Chart with idle, running, escalated (compound with sub-states),
  and wait-pr-merge states."
  (statechart {:initial :idle}
              (state {:id :idle}
                     (transition {:event :start :target :running}))
              (state {:id :running}
                     (transition {:event :error :target :escalated})
                     (transition {:event :done :target :idle})
                     (transition {:event :pr-ready :target :wait-pr-merge}))
              (state {:id :escalated :initial :session-idle}
                     (transition {:event :resolve :target :idle})
                     (state {:id :session-idle}
                            (transition {:event :on-active
                                         :target :session-running}))
                     (state {:id :session-running}
                            (transition {:event :on-session-idle
                                         :target :session-idle})))
              (state {:id :wait-pr-merge}
                     (transition {:event :merge-pr :target :idle}))))

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
            (is (= :session-idle (:sub-state (first result))))
            (is (= 10 (:task-id (first result))))
            (is (= "Esc task" (:task-title (first result))))
            (is (string? (:entered-state-at (first result))))))))

    (testing "filters idle sessions"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (core/start! ::chart {:data {:task-id 5
                                     :task-title "Idle one"}})
        (let [result (core/query-sessions #{:idle})]
          (is (= 1 (count result)))
          (is (= :idle (:state (first result))))
          (is (nil? (:sub-state (first result))))
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
          (is (nil? (:task-title (first result)))))))

    (testing "includes project-dir from session data"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (core/start! ::chart {:data {:task-id 1
                                     :project-dir "/tmp/proj"}})
        (let [result (core/query-sessions #{:idle})]
          (is (= "/tmp/proj" (:project-dir (first result)))))))

    (testing "includes project-name from session data"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (core/start! ::chart {:data {:task-id 1
                                     :project-name "my-project"}})
        (let [result (core/query-sessions #{:idle})]
          (is (= "my-project" (:project-name (first result)))))))

    (testing "returns nil project-name when not in session data"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (core/start! ::chart {:data {:task-id 1}})
        (let [result (core/query-sessions #{:idle})]
          (is (nil? (:project-name (first result)))))))

    (testing "matches :wait-pr-merge sessions"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [sid (core/start! ::chart
                               {:data {:task-id 7
                                       :task-title "PR task"
                                       :pr-num 42
                                       :branch "feat/x"}})]
          (core/send! sid :start)
          (core/send! sid :pr-ready)
          (let [result (core/query-sessions #{:escalated :idle :wait-pr-merge})]
            (is (= 1 (count result)))
            (is (= :wait-pr-merge (:state (first result))))
            (is (= 42 (:pr-num (first result))))
            (is (= "feat/x" (:branch (first result))))))))

    (testing "includes pr-num and branch from session data"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (core/start! ::chart {:data {:task-id 3
                                     :pr-num 99
                                     :branch "main"}})
        (let [result (core/query-sessions #{:idle})]
          (is (= 99 (:pr-num (first result))))
          (is (= "main" (:branch (first result)))))))

    (testing "returns nil :sub-state for non-escalated sessions"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (core/start! ::chart {:data {:task-id 1}})
        (let [result (core/query-sessions #{:idle})]
          (is (nil? (:sub-state (first result)))))))))

(deftest select-priority-state-test
  ;; Tests priority-based state selection: :escalated > :idle > lexicographic.
  ;; Ensures the most actionable state is surfaced in session summaries.
  (let [select @#'core/select-priority-state]
    (testing "select-priority-state"
      (testing "prefers :escalated over other states"
        (is (= :escalated (select #{:escalated :idle})))
        (is (= :escalated (select #{:escalated :running})))
        (is (= :escalated (select #{:escalated :idle :running}))))
      (testing "prefers :idle over :wait-pr-merge"
        (is (= :idle (select #{:idle :wait-pr-merge}))))
      (testing "prefers :idle over non-priority states"
        (is (= :idle (select #{:idle :running})))
        (is (= :idle (select #{:idle :waiting}))))
      (testing "prefers :wait-pr-merge over non-priority states"
        (is (= :wait-pr-merge (select #{:wait-pr-merge :running}))))
      (testing "falls back to lexicographic sort"
        (is (= :running (select #{:running :waiting})))
        (is (= :alpha (select #{:beta :alpha}))))
      (testing "returns single state as-is"
        (is (= :idle (select #{:idle})))
        (is (= :running (select #{:running})))))))

(deftest all-session-summaries-test
  (testing "all-session-summaries"
    (testing "returns empty vec when no sessions"
      (with-clean-engine
        (is (= [] (core/all-session-summaries)))))

    (testing "returns all sessions regardless of state"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [s1 (core/start! ::chart {:data {:task-id 1
                                              :project-dir "/a"}})
              s2 (core/start! ::chart {:data {:task-id 2
                                              :project-dir "/b"}})]
          (core/send! s2 :start) ;; s2 goes to :running
          (let [result (core/all-session-summaries)
                by-id (into {} (map (juxt :session-id identity)) result)]
            (is (= 2 (count result)))
            (is (= :idle (:state (get by-id s1))))
            (is (= :running (:state (get by-id s2))))
            (is (= "/a" (:project-dir (get by-id s1))))
            (is (= "/b" (:project-dir (get by-id s2))))
            (is (= 1 (:task-id (get by-id s1))))
            (is (= 2 (:task-id (get by-id s2))))))))))

(deftest sub-state-test
  ;; Tests for :sub-state derivation in session summaries.
  ;; Verifies that escalated sessions expose :session-idle/:session-running
  ;; and non-escalated sessions have nil :sub-state.
  (testing "sub-state in query-sessions"
    (testing "returns :session-idle for newly escalated session"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [sid (core/start! ::chart {:data {:task-id 1}})]
          (core/send! sid :start)
          (core/send! sid :error)
          (let [result (first (core/query-sessions #{:escalated}))]
            (is (= :escalated (:state result)))
            (is (= :session-idle (:sub-state result)))))))

    (testing "returns :session-running after :on-active"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [sid (core/start! ::chart {:data {:task-id 2}})]
          (core/send! sid :start)
          (core/send! sid :error)
          (core/send! sid :on-active)
          (let [result (first (core/query-sessions #{:escalated}))]
            (is (= :escalated (:state result)))
            (is (= :session-running (:sub-state result)))))))

    (testing "returns :session-idle after :on-session-idle"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [sid (core/start! ::chart {:data {:task-id 3}})]
          (core/send! sid :start)
          (core/send! sid :error)
          (core/send! sid :on-active)
          (core/send! sid :on-session-idle)
          (let [result (first (core/query-sessions #{:escalated}))]
            (is (= :session-idle (:sub-state result))))))))

  (testing "sub-state in all-session-summaries"
    (testing "includes sub-state for escalated and nil for others"
      (with-clean-engine
        (core/register! ::chart test-chart)
        (let [s1 (core/start! ::chart {:data {:task-id 1}})
              s2 (core/start! ::chart {:data {:task-id 2}})]
          (core/send! s1 :start)
          (core/send! s1 :error)
          (core/send! s1 :on-active)
          (let [result (core/all-session-summaries)
                by-id (into {} (map (juxt :session-id identity)) result)]
            (is (= :session-running (:sub-state (get by-id s1))))
            (is (nil? (:sub-state (get by-id s2))))))))))
