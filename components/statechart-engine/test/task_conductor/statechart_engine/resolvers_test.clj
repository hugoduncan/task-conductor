(ns task-conductor.statechart-engine.resolvers-test
  ;; Tests for engine resolvers and mutations.
  ;; Verifies: session listing, chart listing, session state, session history,
  ;; and mutations for start!/send!/stop! via EQL.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.resolvers :as resolvers]
   [task-conductor.statechart-engine.test-helpers :refer [with-clean-state]]))

;;; Test Chart

(defn simple-chart
  "Minimal statechart for testing: idle → running → done.
   Uses (initial ...) pattern matching integration tests."
  []
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition {:event :start :target :running}))
                 (sc/state {:id :running}
                           (sc/transition {:event :finish :target :done}))
                 (sc/final {:id :done})))

;;; Tests

(deftest engine-sessions-resolver-test
  ;; Verifies :engine/sessions returns list of active session IDs.
  (testing "engine-sessions resolver"
    (testing "returns empty vector when no sessions"
      (with-clean-state
        (let [result (graph/query [:engine/sessions])]
          (is (= {:engine/sessions []} result)))))

    (testing "returns session IDs after starting sessions"
      (with-clean-state
        (sc/register! ::test-chart (simple-chart))
        (let [s1 (sc/start! ::test-chart)
              s2 (sc/start! ::test-chart)
              result (graph/query [:engine/sessions])
              sessions (:engine/sessions result)]
          (is (= 2 (count sessions)))
          (is (some #{s1} sessions))
          (is (some #{s2} sessions)))))))

(deftest engine-charts-resolver-test
  ;; Verifies :engine/charts returns list of registered chart names.
  (testing "engine-charts resolver"
    (testing "returns empty vector when no charts"
      (with-clean-state
        (let [result (graph/query [:engine/charts])]
          (is (= {:engine/charts []} result)))))

    (testing "returns chart names after registration"
      (with-clean-state
        (sc/register! ::chart-a (simple-chart))
        (sc/register! ::chart-b (simple-chart))
        (let [result (graph/query [:engine/charts])
              charts (:engine/charts result)]
          (is (= 2 (count charts)))
          (is (some #{::chart-a} charts))
          (is (some #{::chart-b} charts)))))))

(deftest engine-session-state-resolver-test
  ;; Verifies :engine/session-state returns current state configuration.
  (testing "engine-session-state resolver"
    (testing "returns initial state"
      (with-clean-state
        (sc/register! ::state-chart (simple-chart))
        (let [sid (sc/start! ::state-chart)
              result (graph/query {:engine/session-id sid}
                                  [:engine/session-state])]
          (is (= #{:idle} (:engine/session-state result))))))

    (testing "returns updated state after transition"
      (with-clean-state
        (sc/register! ::state-chart2 (simple-chart))
        (let [sid (sc/start! ::state-chart2)]
          (sc/send! sid :start)
          (let [result (graph/query {:engine/session-id sid}
                                    [:engine/session-state])]
            (is (= #{:running} (:engine/session-state result)))))))))

(deftest engine-session-history-resolver-test
  ;; Verifies :engine/session-history returns state transition history.
  (testing "engine-session-history resolver"
    (testing "returns initial history entry"
      (with-clean-state
        (sc/register! ::hist-chart (simple-chart))
        (let [sid (sc/start! ::hist-chart)
              result (graph/query {:engine/session-id sid}
                                  [:engine/session-history])
              history (:engine/session-history result)]
          (is (= 1 (count history)))
          (is (= #{:idle} (:state (first history))))
          (is (nil? (:event (first history)))))))

    (testing "returns history entries after transitions"
      (with-clean-state
        (sc/register! ::hist-chart2 (simple-chart))
        (let [sid (sc/start! ::hist-chart2)]
          (sc/send! sid :start)
          (let [result (graph/query {:engine/session-id sid}
                                    [:engine/session-history])
                history (:engine/session-history result)]
            (is (= 2 (count history)))
            (is (= #{:idle} (:state (first history))))
            (is (nil? (:event (first history))))
            (is (= :start (:event (second history))))
            (is (= #{:running} (:state (second history))))))))))

;;; Mutation Tests

(deftest engine-start-mutation-test
  ;; Verifies engine/start! mutation creates sessions.
  (testing "engine/start! mutation"
    (testing "creates a new session"
      (with-clean-state
        (sc/register! ::mut-chart (simple-chart))
        (let [result (graph/query
                      [`(resolvers/engine-start!
                         {:engine/chart-id ::mut-chart})])
              sid (get result `resolvers/engine-start!)]
          (is (string? (:engine/session-id sid)))
          (is (some #{(:engine/session-id sid)}
                    (:engine/sessions (graph/query [:engine/sessions])))))))

    (testing "throws for non-existent chart"
      (with-clean-state
        (is (thrown? clojure.lang.ExceptionInfo
                     (graph/query
                      [`(resolvers/engine-start!
                         {:engine/chart-id ::nonexistent})])))))))

(deftest engine-send-mutation-test
  ;; Verifies engine/send! mutation transitions state.
  (testing "engine/send! mutation"
    (testing "transitions state with event"
      (with-clean-state
        (sc/register! ::send-chart (simple-chart))
        (let [sid (sc/start! ::send-chart)
              result (graph/query
                      [`(resolvers/engine-send!
                         {:engine/session-id ~sid
                          :engine/event :start})])
              new-state (get-in result
                                [`resolvers/engine-send! :engine/session-state])]
          (is (= #{:running} new-state)))))

    (testing "throws for non-existent session"
      (with-clean-state
        (is (thrown? clojure.lang.ExceptionInfo
                     (graph/query
                      [`(resolvers/engine-send!
                         {:engine/session-id "invalid"
                          :engine/event :start})])))))))

(deftest engine-stop-mutation-test
  ;; Verifies engine/stop! mutation removes sessions.
  (testing "engine/stop! mutation"
    (testing "removes the session"
      (with-clean-state
        (sc/register! ::stop-chart (simple-chart))
        (let [sid (sc/start! ::stop-chart)
              _ (is (some #{sid}
                          (:engine/sessions (graph/query [:engine/sessions]))))
              result (graph/query
                      [`(resolvers/engine-stop!
                         {:engine/session-id ~sid})])
              returned-id (get-in result
                                  [`resolvers/engine-stop! :engine/session-id])]
          (is (= sid returned-id))
          (is (empty? (:engine/sessions (graph/query [:engine/sessions])))))))

    (testing "throws for non-existent session"
      (with-clean-state
        (is (thrown? clojure.lang.ExceptionInfo
                     (graph/query
                      [`(resolvers/engine-stop!
                         {:engine/session-id "invalid"})])))))))
