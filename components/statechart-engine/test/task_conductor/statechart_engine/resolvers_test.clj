(ns task-conductor.statechart-engine.resolvers-test
  ;; Tests for engine introspection resolvers.
  ;; Verifies: session listing, chart listing, session state, session history via EQL.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.resolvers :as resolvers]))

;;; Test Helpers

(defmacro with-clean-state
  "Execute body with fresh engine and graph state."
  [& body]
  `(do
     (sc/reset-engine!)
     (graph/reset-graph!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         (sc/reset-engine!)
         (graph/reset-graph!)))))

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
