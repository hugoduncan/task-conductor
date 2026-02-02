(ns task-conductor.statechart-engine.resolvers-test
  ;; Tests for engine resolvers and mutations.
  ;; Verifies: session listing, chart listing, session state, session history,
  ;; mutations for start!/send!/stop!, and dev-env hook registration via EQL.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.protocol :as protocol]
   [task-conductor.dev-env.registry :as dev-env-registry]
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
                                [`resolvers/engine-send!
                                 :engine/session-state])]
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

;;; Dev-env Hook Registration Tests

(defn waiting-chart
  "Statechart for testing hook integration: idle → waiting → received.
   Transitions on :dev-env-closed event to :received (non-final for testing)."
  []
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition
                            {:event :start-waiting :target :waiting}))
                 (sc/state {:id :waiting}
                           (sc/transition
                            {:event :dev-env-closed :target :received}))
                 (sc/state {:id :received})))

(deftest engine-register-dev-env-hook-test
  ;; Verifies hook registration connects dev-env callbacks to statechart events.
  (testing "engine-register-dev-env-hook!"
    (testing "registers hook and returns hook-id"
      (with-clean-state
        (let [dev-env (protocol/make-noop-dev-env)
              dev-env-id (dev-env-registry/register! dev-env :test)
              _ (sc/register! ::hook-chart (waiting-chart))
              session-id (sc/start! ::hook-chart)
              result (graph/query
                      [`(resolvers/engine-register-dev-env-hook!
                         {:dev-env/id ~dev-env-id
                          :dev-env/hook-type :on-close
                          :engine/session-id ~session-id
                          :engine/event :dev-env-closed})])
              hook-id (get-in result [`resolvers/engine-register-dev-env-hook!
                                      :engine/hook-id])]
          (is (some? hook-id))
          (is (uuid? hook-id)))))

    (testing "hook callback sends event to statechart"
      (with-clean-state
        (let [dev-env (protocol/make-noop-dev-env)
              dev-env-id (dev-env-registry/register! dev-env :test)
              _ (sc/register! ::hook-chart2 (waiting-chart))
              session-id (sc/start! ::hook-chart2)
              _ (sc/send! session-id :start-waiting)
              _ (is (= #{:waiting} (sc/current-state session-id)))
              result (graph/query
                      [`(resolvers/engine-register-dev-env-hook!
                         {:dev-env/id ~dev-env-id
                          :dev-env/hook-type :on-close
                          :engine/session-id ~session-id
                          :engine/event :dev-env-closed})])
              hook-id (get-in result [`resolvers/engine-register-dev-env-hook!
                                      :engine/hook-id])
              ;; Get the callback from the noop dev-env's hooks atom
              callback (get-in @(:hooks dev-env) [hook-id :callback])]
          ;; Invoke the callback as dev-env would
          (callback {:session-id "test" :timestamp (java.time.Instant/now)})
          ;; Statechart should have transitioned to :done
          (is (= #{:received} (sc/current-state session-id))))))

    (testing "returns error for non-existent dev-env"
      (with-clean-state
        (sc/register! ::hook-chart3 (waiting-chart))
        (let [session-id (sc/start! ::hook-chart3)
              result (graph/query
                      [`(resolvers/engine-register-dev-env-hook!
                         {:dev-env/id "missing"
                          :dev-env/hook-type :on-close
                          :engine/session-id ~session-id
                          :engine/event :dev-env-closed})])
              hook-result (get-in
                           result
                           [`resolvers/engine-register-dev-env-hook!
                            :engine/hook-id])]
          (is (= :not-found (:error hook-result)))
          (is (string? (:message hook-result))))))))

(deftest engine-unregister-dev-env-hook-test
  ;; Verifies hook unregistration removes tracking.
  (testing "engine-unregister-dev-env-hook!"
    (testing "returns true when hook existed"
      (with-clean-state
        (let [dev-env (protocol/make-noop-dev-env)
              dev-env-id (dev-env-registry/register! dev-env :test)
              _ (sc/register! ::unreg-chart (waiting-chart))
              session-id (sc/start! ::unreg-chart)
              _ (graph/query
                 [`(resolvers/engine-register-dev-env-hook!
                    {:dev-env/id ~dev-env-id
                     :dev-env/hook-type :on-close
                     :engine/session-id ~session-id
                     :engine/event :dev-env-closed})])
              result (graph/query
                      [`(resolvers/engine-unregister-dev-env-hook!
                         {:dev-env/id ~dev-env-id
                          :dev-env/hook-type :on-close})])
              unregistered? (get-in
                             result
                             [`resolvers/engine-unregister-dev-env-hook!
                              :engine/unregistered?])]
          (is (true? unregistered?)))))

    (testing "returns false when hook did not exist"
      (with-clean-state
        (let [result (graph/query
                      [`(resolvers/engine-unregister-dev-env-hook!
                         {:dev-env/id "any"
                          :dev-env/hook-type :on-close})])
              unregistered? (get-in
                             result
                             [`resolvers/engine-unregister-dev-env-hook!
                              :engine/unregistered?])]
          (is (false? unregistered?)))))))
