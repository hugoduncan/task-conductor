(ns task-conductor.statechart-engine.eql-action-test
  ;; Integration tests for EQL-based action execution in statecharts.
  ;; Tests that action :expr expressions route correctly through EQLExecutionModel:
  ;; - vector → EQL query
  ;; - list with symbol → mutation
  ;; - fn → direct call (escape hatch)
  ;; Also tests error propagation and history tracking.
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.fulcrologic.statecharts.protocols :as sp]
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.statechart-engine.core :as core]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.resolvers :as resolvers]
   [task-conductor.statechart-engine.test-helpers :refer [with-clean-state]]))

;;; Test Resolvers and Mutations

(defonce ^{:doc "Atom to capture resolver/mutation calls for testing."} call-log
  (atom []))

(graph/defresolver test-value []
  {::pco/output [:test/value]}
  (swap! call-log conj [:resolver :test/value])
  {:test/value 42})

(graph/defresolver test-parameterized [{:test/keys [input]}]
  {::pco/input [:test/input]
   ::pco/output [:test/doubled]}
  (swap! call-log conj [:resolver :test/doubled input])
  {:test/doubled (* 2 input)})

(graph/defresolver test-error []
  {::pco/output [:test/error]}
  (swap! call-log conj [:resolver :test/error])
  (throw (ex-info "Test error from resolver" {:type :test-error})))

(graph/defmutation test-mutate! [{:test/keys [data]}]
  {::pco/output [:test/result]}
  (swap! call-log conj [:mutation :test/mutate! data])
  {:test/result (str "processed:" data)})

(graph/defmutation
  test-capture-session!
  [{:engine/keys [session-id] :test/keys [data]}]
  {::pco/output [:test/result]}
  (swap! call-log conj [:mutation :test/capture-session! session-id data])
  {:test/result {:session-id session-id :data data}})

(graph/defmutation test-no-params! [{:engine/keys [session-id]}]
  {::pco/output [:test/result]}
  (swap! call-log conj [:mutation :test/no-params! session-id])
  {:test/result :success})

(defn register-test-ops!
  "Register test resolvers and mutations."
  []
  (graph/register! [test-value test-parameterized test-error test-mutate!
                    test-capture-session! test-no-params!]))

(defn reset-call-log! []
  (reset! call-log []))

;;; Test Statecharts

(defn query-action-chart
  "Chart with on-entry action that executes EQL query."
  []
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition {:event :go :target :active}))
                 (sc/state {:id :active}
                           (sc/on-entry {}
                                        (sc/action {:expr [:test/value]})))))

(defn mutation-action-chart
  "Chart with on-entry action that executes mutation."
  []
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition {:event :go :target :active}))
                 (sc/state {:id :active}
                           (sc/on-entry {}
                                        (sc/action {:expr `(test-mutate!
                                                            {:test/data "hello"})})))))

(defn fn-escape-chart
  "Chart with on-entry action using function escape hatch."
  [action-fn]
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition {:event :go :target :active}))
                 (sc/state {:id :active}
                           (sc/on-entry {}
                                        (sc/action {:expr action-fn})))))

(defn multi-action-chart
  "Chart with multiple actions in sequence."
  []
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition {:event :go :target :active}))
                 (sc/state {:id :active}
                           (sc/on-entry {}
                                        (sc/action {:expr [:test/value]})
                                        (sc/action {:expr `(test-mutate!
                                                            {:test/data "first"})})
                                        (sc/action {:expr `(test-mutate!
                                                            {:test/data "second"})})))))

(defn error-action-chart
  "Chart with action that queries resolver that throws."
  []
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition {:event :go :target :active}))
                 (sc/state {:id :active}
                           (sc/on-entry {}
                                        (sc/action {:expr [:test/error]})))))

(defn session-id-injection-chart
  "Chart with action that captures injected session-id."
  []
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition {:event :go :target :active}))
                 (sc/state {:id :active}
                           (sc/on-entry {}
                                        (sc/action
                                         {:expr '(task-conductor.statechart-engine.eql-action-test/test-capture-session!
                                                  {:test/data "injected"})})))))

(defn no-params-chart
  "Chart with action that calls mutation without params."
  []
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition {:event :go :target :active}))
                 (sc/state {:id :active}
                           (sc/on-entry {}
                                        (sc/action
                                         {:expr '(task-conductor.statechart-engine.eql-action-test/test-no-params!)})))))

;;; Tests

(deftest eql-query-action-test
  ;; Verifies that action with vector expression executes EQL query.
  (testing "EQL query action"
    (testing "executes query on state entry"
      (with-clean-state
        (register-test-ops!)
        (reset-call-log!)
        (sc/register! ::query-chart (query-action-chart))
        (let [sid (sc/start! ::query-chart)]
          (is (= #{:idle} (sc/current-state sid)))
          (is (empty? @call-log) "No queries before transition")
          (sc/send! sid :go)
          (is (= #{:active} (sc/current-state sid)))
          (is (= [[:resolver :test/value]] @call-log)
              "Query executed on entry"))))))

(deftest mutation-action-test
  ;; Verifies that action with list expression executes mutation.
  (testing "mutation action"
    (testing "executes mutation on state entry"
      (with-clean-state
        (register-test-ops!)
        (reset-call-log!)
        (sc/register! ::mutation-chart (mutation-action-chart))
        (let [sid (sc/start! ::mutation-chart)]
          (is (empty? @call-log) "No mutations before transition")
          (sc/send! sid :go)
          (is (= #{:active} (sc/current-state sid)))
          (is (= [[:mutation :test/mutate! "hello"]] @call-log)
              "Mutation executed on entry"))))))

(deftest fn-escape-hatch-test
  ;; Verifies that action with function expression calls function directly.
  (testing "function escape hatch"
    (testing "calls function with env and data"
      (with-clean-state
        (register-test-ops!)
        (reset-call-log!)
        (let [captured (atom nil)
              action-fn (fn [env data]
                          (reset! captured {:env-keys (keys env)
                                            :data data})
                          :fn-result)]
          (sc/register! ::fn-chart (fn-escape-chart action-fn))
          (let [sid (sc/start! ::fn-chart)]
            (sc/send! sid :go)
            (is (= #{:active} (sc/current-state sid)))
            (is (some? @captured) "Function was called")
            (is (contains? (set (:env-keys @captured))
                           :com.fulcrologic.statecharts/data-model)
                "Env contains statechart context")))))))

(deftest multi-action-sequence-test
  ;; Verifies multiple actions execute in order.
  (testing "multiple actions in sequence"
    (testing "execute in definition order"
      (with-clean-state
        (register-test-ops!)
        (reset-call-log!)
        (sc/register! ::multi-chart (multi-action-chart))
        (let [sid (sc/start! ::multi-chart)]
          (sc/send! sid :go)
          (is (= #{:active} (sc/current-state sid)))
          (is (= [[:resolver :test/value]
                  [:mutation :test/mutate! "first"]
                  [:mutation :test/mutate! "second"]]
                 @call-log)
              "Actions executed in order"))))))

(deftest error-propagation-test
  ;; Verifies errors from EQL queries are handled by the statechart framework.
  ;; Fulcrologic statecharts catch action errors and send :error.execution events
  ;; rather than propagating exceptions. The state transition still completes.
  (testing "error propagation"
    (testing "resolver error is handled internally, transition completes"
      (with-clean-state
        (register-test-ops!)
        (reset-call-log!)
        (sc/register! ::error-chart (error-action-chart))
        (let [sid (sc/start! ::error-chart)]
          ;; Action error doesn't prevent state transition
          (sc/send! sid :go)
          (is (= #{:active} (sc/current-state sid))
              "State transition completed despite action error")
          (is (= [[:resolver :test/error]] @call-log)
              "Resolver was called before error"))))))

(deftest history-with-eql-actions-test
  ;; Verifies history tracking works with EQL actions.
  (testing "history with EQL actions"
    (testing "records state transitions correctly"
      (with-clean-state
        (register-test-ops!)
        (reset-call-log!)
        (sc/register! ::hist-chart (query-action-chart))
        (let [sid (sc/start! ::hist-chart)]
          (sc/send! sid :go)
          (let [history (sc/history sid)]
            (is (= 2 (count history)))
            (is (= #{:idle} (:state (first history))))
            (is (nil? (:event (first history))))
            (is (= :go (:event (second history))))
            (is (= #{:active} (:state (second history))))))))))

(deftest engine-resolver-in-action-test
  ;; Verifies actions can query engine resolvers (self-introspection).
  (testing "engine resolver in action"
    (testing "can query engine sessions from within action"
      (with-clean-state
        (let [captured-sessions (atom nil)
              introspect-chart
              (sc/statechart {}
                             (sc/initial {}
                                         (sc/transition {:target :idle}))
                             (sc/state {:id :idle}
                                       (sc/transition
                                        {:event :check :target :checked}))
                             (sc/state {:id :checked}
                                       (sc/on-entry {}
                                                    (sc/action
                                                     {:expr (fn [_env _data]
                                                              (reset!
                                                               captured-sessions
                                                               (graph/query
                                                                [:engine/sessions])))}))))]
          (sc/register! ::intro-chart introspect-chart)
          (let [sid (sc/start! ::intro-chart)]
            (sc/send! sid :check)
            (is (some? @captured-sessions))
            (is (contains? @captured-sessions :engine/sessions))
            (is (some #{sid} (:engine/sessions @captured-sessions))
                "Session can see itself via EQL")))))))

(deftest engine-mutation-in-action-test
  ;; Verifies actions can call engine mutations.
  (testing "engine mutation in action"
    (testing "can start another session from within action"
      (with-clean-state
        (let [simple-chart (sc/statechart {}
                                          (sc/initial {}
                                                      (sc/transition
                                                       {:target :done}))
                                          (sc/final {:id :done}))
              spawner-chart
              (sc/statechart {}
                             (sc/initial {}
                                         (sc/transition {:target :idle}))
                             (sc/state {:id :idle}
                                       (sc/transition
                                        {:event :spawn :target :spawned}))
                             (sc/state {:id :spawned}
                                       (sc/on-entry {}
                                                    (sc/action
                                                     {:expr `(resolvers/engine-start!
                                                              {:engine/chart-id ::spawnable})}))))]
          (sc/register! ::spawnable simple-chart)
          (sc/register! ::spawner spawner-chart)
          (let [sid (sc/start! ::spawner)]
            (is (= 1 (count (sc/list-sessions))))
            (sc/send! sid :spawn)
            (is (= 2 (count (sc/list-sessions)))
                "Action spawned new session")))))))

(defn unknown-expr-chart
  "Chart with action using unsupported expression type (string)."
  []
  (sc/statechart {}
                 (sc/initial {}
                             (sc/transition {:target :idle}))
                 (sc/state {:id :idle}
                           (sc/transition {:event :go :target :bad}))
                 (sc/state {:id :bad}
                           (sc/on-entry {}
                                        (sc/action
                                         {:expr "unsupported-string"})))))

(deftest session-id-injection-test
  ;; Verifies that mutations executed from statechart actions receive
  ;; the session-id automatically injected into their params.
  (testing "session-id injection"
    (testing "injects session-id into mutation params"
      (with-clean-state
        (register-test-ops!)
        (reset-call-log!)
        (sc/register! ::inject-chart (session-id-injection-chart))
        (let [sid (sc/start! ::inject-chart)]
          (sc/send! sid :go)
          (is (= 1 (count @call-log)))
          (let [[_type _name session-id data] (first @call-log)]
            (is (= sid session-id) "Session ID matches statechart session")
            (is (= "injected" data))))))

    (testing "handles mutations without params"
      (with-clean-state
        (register-test-ops!)
        (reset-call-log!)
        (sc/register! ::no-params-chart (no-params-chart))
        (let [sid (sc/start! ::no-params-chart)]
          (sc/send! sid :go)
          (is (= 1 (count @call-log)))
          (let [[_type _name session-id] (first @call-log)]
            (is
             (= sid session-id)
             "Session ID injected for param-less mutation")))))))

(deftest unknown-expression-type-test
  ;; Verifies EQLExecutionModel throws on unsupported expression types.
  ;; Supported: vector (EQL query), seq/list (mutation), fn (escape hatch).
  ;; Unsupported: strings, keywords, numbers, maps, etc.
  (testing "unknown expression type"
    (testing "throws with message and ex-data for string"
      (let [model (core/->EQLExecutionModel)]
        (try
          (sp/run-expression! model {} "unsupported")
          (is false "Expected exception to be thrown")
          (catch clojure.lang.ExceptionInfo e
            (is (= "Unknown action expression type" (.getMessage e)))
            (is (= "unsupported" (:expression (ex-data e))))
            (is (= String (:type (ex-data e))))))))

    (testing "throws with message and ex-data for keyword"
      (let [model (core/->EQLExecutionModel)]
        (try
          (sp/run-expression! model {} :bad-keyword)
          (is false "Expected exception to be thrown")
          (catch clojure.lang.ExceptionInfo e
            (is (= "Unknown action expression type" (.getMessage e)))
            (is (= :bad-keyword (:expression (ex-data e))))
            (is (= clojure.lang.Keyword (:type (ex-data e))))))))

    (testing "throws with message and ex-data for number"
      (let [model (core/->EQLExecutionModel)]
        (try
          (sp/run-expression! model {} 42)
          (is false "Expected exception to be thrown")
          (catch clojure.lang.ExceptionInfo e
            (is (= "Unknown action expression type" (.getMessage e)))
            (is (= 42 (:expression (ex-data e))))
            (is (= Long (:type (ex-data e))))))))

    (testing "string in action causes statechart error handling"
      (with-clean-state
        (sc/register! ::unknown-chart (unknown-expr-chart))
        (let [sid (sc/start! ::unknown-chart)]
          (is (= #{:idle} (sc/current-state sid)))
          ;; The statechart framework catches action errors and transitions anyway
          (sc/send! sid :go)
          (is (= #{:bad} (sc/current-state sid))
              "State transition completes (framework catches action errors)"))))))
