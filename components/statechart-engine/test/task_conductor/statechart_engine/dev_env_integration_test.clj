(ns task-conductor.statechart-engine.dev-env-integration-test
  ;; Integration tests validating the complete flow:
  ;; statechart starts dev-env session,
  ;; registers hook, receives completion event, and queries result. Tests that
  ;; the statechart engine can orchestrate dev-env sessions via EQL.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.protocol :as dev-env-protocol]
   [task-conductor.dev-env.registry :as dev-env-registry]
   [task-conductor.dev-env.resolvers :as dev-env-resolvers]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.resolvers :as resolvers]
   [task-conductor.statechart-engine.test-helpers :refer [with-clean-state]]))

;;; Mock Dev-Env Implementation

(defrecord MockDevEnv [transcript-data hooks sessions]
  ;; A mock dev-env that tracks sessions and allows triggering hooks.
  ;; - transcript-data: atom with map of session-id -> transcript string
  ;; - hooks: atom with map of hook-type -> [callbacks...]
  ;; - sessions: atom with map of session-id -> session state

  dev-env-protocol/DevEnv
  (start-session [_ session-id opts]
    (swap! sessions assoc session-id {:state :running :opts opts})
    {:session-id session-id :handle :mock})

  (register-hook [_ session-id hook-type callback]
    (let [hook-id (java.util.UUID/randomUUID)]
      (swap! hooks update hook-type (fnil conj []) {:id hook-id
                                                    :session-id session-id
                                                    :callback callback})
      hook-id))

  (query-transcript [_ session-id]
    (get @transcript-data session-id))

  (query-events [_ session-id]
    (when-let [session (get @sessions session-id)]
      [{:type :session :session-id session-id :state (:state session)}]))

  (close-session [_ session-id]
    (if (contains? @sessions session-id)
      (do
        (swap! sessions dissoc session-id)
        true)
      false))

  (connected? [_]
    true))

(defn make-mock-dev-env
  "Create a MockDevEnv for testing."
  []
  (->MockDevEnv (atom {}) (atom {}) (atom {})))

(defn trigger-hook!
  "Trigger all callbacks registered for a hook-type on the mock dev-env."
  [mock-dev-env hook-type context]
  (doseq [{:keys [callback]} (get @(:hooks mock-dev-env) hook-type)]
    (callback context)))

(defn set-transcript!
  "Set the transcript for a session in the mock dev-env."
  [mock-dev-env session-id transcript]
  (swap! (:transcript-data mock-dev-env) assoc session-id transcript))

;;; Statechart Definition

(defn orchestrator-chart
  "Statechart that orchestrates a dev-env session.
   States: :idle -> :running -> :completed
   On :running entry: starts session.
   On :on-close hook: transitions to :completed."
  [dev-env-id]
  (let [start-expr
        `(dev-env-resolvers/dev-env-start-session!
          {:dev-env/id ~dev-env-id
           :dev-env/session-id "test-session"
           :dev-env/opts {:dir "/tmp"}})]
    (sc/statechart {}
                   (sc/initial {}
                               (sc/transition {:target :idle}))
                   (sc/state {:id :idle}
                             (sc/transition
                              {:event :start
                               :target :running}))
                   (sc/state {:id :running}
                             (sc/on-entry {}
                                          (sc/action
                                           {:expr start-expr}))
                             (sc/transition
                              {:event :session-closed
                               :target :completed}))
                   (sc/state {:id :completed}))))

(defn orchestrator-with-hook-chart
  "Statechart that registers a hook in :running on-entry.
   The hook-id is captured via session-id in context."
  [dev-env-id session-id-atom]
  (let [start-expr
        `(dev-env-resolvers/dev-env-start-session!
          {:dev-env/id ~dev-env-id
           :dev-env/session-id "test-session"
           :dev-env/opts {:dir "/tmp"}})
        hook-fn
        (fn [_env _data]
          (graph/query
           [`(resolvers/engine-register-dev-env-hook!
              {:dev-env/id ~dev-env-id
               :dev-env/hook-type :on-close
               :engine/session-id ~(deref session-id-atom)
               :engine/event :session-closed})]))]
    (sc/statechart {}
                   (sc/initial {}
                               (sc/transition {:target :idle}))
                   (sc/state {:id :idle}
                             (sc/transition
                              {:event :start
                               :target :running}))
                   (sc/state {:id :running}
                             (sc/on-entry {}
                                          (sc/action
                                           {:expr start-expr})
                                          (sc/action
                                           {:expr hook-fn}))
                             (sc/transition
                              {:event :session-closed
                               :target :completed}))
                   (sc/state {:id :completed}))))

;;; Tests

(deftest dev-env-session-start-test
  ;; Verifies that a statechart can start a dev-env session via EQL mutation.
  (testing "dev-env session start via statechart"
    (testing "starts session on state entry"
      (with-clean-state
        (dev-env-resolvers/register-resolvers!)
        (let [mock-dev-env (make-mock-dev-env)
              dev-env-id (dev-env-registry/register! mock-dev-env :mock)]
          (sc/register! ::start-chart (orchestrator-chart dev-env-id))
          (let [sid (sc/start! ::start-chart)]
            (is (= #{:idle} (sc/current-state sid)))
            (is (empty? @(:sessions mock-dev-env)) "No sessions before start")
            (sc/send! sid :start)
            (is (= #{:running} (sc/current-state sid)))
            (is (= 1 (count @(:sessions mock-dev-env))) "Session started")
            (is (contains? @(:sessions mock-dev-env) "test-session"))))))))

(deftest dev-env-hook-triggers-transition-test
  ;; Verifies that a dev-env hook callback triggers a statechart transition.
  (testing "dev-env hook triggers transition"
    (testing "on-close hook sends event to statechart"
      (with-clean-state
        (dev-env-resolvers/register-resolvers!)
        (let [mock-dev-env (make-mock-dev-env)
              dev-env-id (dev-env-registry/register! mock-dev-env :mock)
              session-id-atom (atom nil)]
          ;; Register chart and start session
          (sc/register!
           ::hook-chart
           (orchestrator-with-hook-chart dev-env-id session-id-atom))
          (let [sid (sc/start! ::hook-chart)]
            ;; Store session-id for hook registration
            (reset! session-id-atom sid)
            (sc/send! sid :start)
            (is (= #{:running} (sc/current-state sid)))

            ;; Simulate dev-env closing the session
            (trigger-hook! mock-dev-env :on-close
                           {:session-id "test-session"
                            :timestamp (java.time.Instant/now)
                            :reason :user-exit})

            ;; Verify statechart transitioned to completed
            (is (= #{:completed} (sc/current-state sid)))))))))

(deftest dev-env-transcript-query-test
  ;; Verifies that a statechart can query the transcript from a dev-env.
  (testing "dev-env transcript query"
    (testing "can query transcript via EQL"
      (with-clean-state
        (dev-env-resolvers/register-resolvers!)
        (let [mock-dev-env (make-mock-dev-env)
              dev-env-id (dev-env-registry/register! mock-dev-env :mock)]
          ;; Set up transcript
          (set-transcript!
           mock-dev-env
           "test-session"
           "User: Hello\nAssistant: Hi")

          ;; Query via EQL - mutations return under the mutation key
          (let [result (graph/query
                        [`(dev-env-resolvers/dev-env-query-transcript!
                           {:dev-env/id ~dev-env-id
                            :dev-env/session-id "test-session"})])]
            (is (= "User: Hello\nAssistant: Hi"
                   (get-in result [`dev-env-resolvers/dev-env-query-transcript!
                                   :dev-env/transcript])))))))))

(deftest full-orchestration-flow-test
  ;; Complete integration test: statechart orchestrates
  ;; dev-env session lifecycle.
  ;; 1. Start in :idle state
  ;; 2. On :start, enter :running, start dev-env
  ;;    session, register hook
  ;; 3. Dev-env completes, fires :on-close hook
  ;; 4. Receives :session-closed, transitions to
  ;;    :completed
  ;; 5. Query transcript shows expected content
  (testing "full orchestration flow"
    (testing "complete dev-env lifecycle via statechart"
      (with-clean-state
        (dev-env-resolvers/register-resolvers!)
        (let [mock-dev-env (make-mock-dev-env)
              dev-env-id
              (dev-env-registry/register!
               mock-dev-env :mock)
              session-id-atom (atom nil)
              captured-transcript (atom nil)
              start-expr
              `(dev-env-resolvers/dev-env-start-session!
                {:dev-env/id ~dev-env-id
                 :dev-env/session-id "full-test"
                 :dev-env/opts {}})
              hook-fn
              (fn [_env _data]
                (graph/query
                 [`(resolvers/engine-register-dev-env-hook!
                    {:dev-env/id ~dev-env-id
                     :dev-env/hook-type :on-close
                     :engine/session-id
                     ~(deref session-id-atom)
                     :engine/event
                     :session-closed})]))
              transcript-sym
              `dev-env-resolvers/dev-env-query-transcript!
              transcript-fn
              (fn [_env _data]
                (let [result
                      (graph/query
                       [`(~transcript-sym
                          {:dev-env/id ~dev-env-id
                           :dev-env/session-id
                           "full-test"})])]
                  (reset!
                   captured-transcript
                   (get-in
                    result
                    [transcript-sym
                     :dev-env/transcript]))))
              full-chart
              (sc/statechart
               {}
               (sc/initial {}
                           (sc/transition
                            {:target :idle}))
               (sc/state
                {:id :idle}
                (sc/transition
                 {:event :start
                  :target :running}))
               (sc/state
                {:id :running}
                (sc/on-entry {}
                             (sc/action
                              {:expr start-expr})
                             (sc/action
                              {:expr hook-fn}))
                (sc/transition
                 {:event :session-closed
                  :target :completed}))
               (sc/state
                {:id :completed}
                (sc/on-entry {}
                             (sc/action
                              {:expr transcript-fn}))))]

          (sc/register! ::full-chart full-chart)
          (let [sid (sc/start! ::full-chart)]
            (reset! session-id-atom sid)

            ;; 1. Verify initial state
            (is (= #{:idle}
                   (sc/current-state sid)))

            ;; 2. Start the flow
            (sc/send! sid :start)
            (is (= #{:running}
                   (sc/current-state sid)))
            (is (= 1
                   (count
                    @(:sessions mock-dev-env))))

            ;; 3. Set transcript before completion
            (set-transcript!
             mock-dev-env
             "full-test"
             "Task completed")

            ;; 4. Simulate dev-env completion
            (trigger-hook!
             mock-dev-env :on-close
             {:session-id "full-test"
              :timestamp
              (java.time.Instant/now)
              :reason :user-exit})

            ;; 5. Verify transition and transcript
            (is (= #{:completed}
                   (sc/current-state sid)))
            (is (= "Task completed"
                   @captured-transcript))))))))
