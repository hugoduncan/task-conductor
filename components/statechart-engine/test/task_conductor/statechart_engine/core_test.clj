(ns task-conductor.statechart-engine.core-test
  ;; Test chart registration, session lifecycle, and event processing.
  ;; Uses a simple two-state chart: :off -> :on via :toggle event.
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.fulcrologic.statecharts.elements :refer [state transition]]
   [com.fulcrologic.statecharts.chart :refer [statechart]]
   [task-conductor.statechart-engine.core :as core]))

(defmacro with-clean-engine
  "Execute body with a fresh engine state, resetting before and after."
  [& body]
  `(do
     (core/reset-engine!)
     (try
       ~@body
       (finally
         (core/reset-engine!)))))

(def simple-chart
  "Two-state chart: :off <-> :on via :toggle event."
  (statechart {}
              (state {:id :off}
                     (transition {:event :toggle :target :on}))
              (state {:id :on}
                     (transition {:event :toggle :target :off}))))

(deftest register!-test
  (testing "register!"
    (testing "registers a new chart successfully"
      (with-clean-engine
        (is (= {:ok ::test-chart}
               (core/register! ::test-chart simple-chart)))))

    (testing "returns error when chart name already registered"
      (with-clean-engine
        (core/register! ::duplicate simple-chart)
        (is (= {:error :already-registered}
               (core/register! ::duplicate simple-chart)))))))

(deftest unregister!-test
  (testing "unregister!"
    (testing "removes a registered chart"
      (with-clean-engine
        (core/register! ::to-remove simple-chart)
        (is (= {:ok ::to-remove}
               (core/unregister! ::to-remove)))))

    (testing "returns error when chart not found"
      (with-clean-engine
        (is (= {:error :not-found}
               (core/unregister! ::nonexistent)))))))

(deftest start!-test
  (testing "start!"
    (testing "starts a session for registered chart"
      (with-clean-engine
        (core/register! ::startable simple-chart)
        (let [result (core/start! ::startable)]
          (is (contains? result :ok))
          (is (string? (:ok result)))
          (is (uuid? (parse-uuid (:ok result)))))))

    (testing "returns error when chart not registered"
      (with-clean-engine
        (is (= {:error :chart-not-found}
               (core/start! ::not-registered)))))))

(deftest stop!-test
  (testing "stop!"
    (testing "stops an active session"
      (with-clean-engine
        (core/register! ::stoppable simple-chart)
        (let [{:keys [ok]} (core/start! ::stoppable)
              session-id   ok]
          (is (= {:ok session-id}
                 (core/stop! session-id))))))

    (testing "returns error when session not found"
      (with-clean-engine
        (is (= {:error :session-not-found}
               (core/stop! "nonexistent-session")))))))

(deftest send!-test
  (testing "send!"
    (testing "transitions state on valid event"
      (with-clean-engine
        (core/register! ::toggle-chart simple-chart)
        (let [{:keys [ok]} (core/start! ::toggle-chart)
              session-id   ok
              result       (core/send! session-id :toggle)]
          (is (contains? result :ok))
          (is (contains? (:ok result) :on)))))

    (testing "toggles back to original state"
      (with-clean-engine
        (core/register! ::toggle-back simple-chart)
        (let [{:keys [ok]} (core/start! ::toggle-back)
              session-id   ok
              _            (core/send! session-id :toggle)
              result       (core/send! session-id :toggle)]
          (is (contains? (:ok result) :off)))))

    (testing "returns error when session not found"
      (with-clean-engine
        (is (= {:error :session-not-found}
               (core/send! "nonexistent" :toggle)))))))

;;; Introspection Tests

(deftest state-test
  (testing "state"
    (testing "returns current configuration for active session"
      (with-clean-engine
        (core/register! ::state-chart simple-chart)
        (let [{:keys [ok]} (core/start! ::state-chart)
              session-id   ok
              result       (core/state session-id)]
          (is (contains? result :ok))
          (is (set? (:ok result)))
          (is (contains? (:ok result) :off)))))

    (testing "reflects state after transition"
      (with-clean-engine
        (core/register! ::state-trans simple-chart)
        (let [{:keys [ok]} (core/start! ::state-trans)
              session-id   ok
              _            (core/send! session-id :toggle)
              result       (core/state session-id)]
          (is (contains? (:ok result) :on)))))

    (testing "returns error when session not found"
      (with-clean-engine
        (is (= {:error :session-not-found}
               (core/state "no-such-session")))))))

(deftest list-sessions-test
  (testing "list-sessions"
    (testing "returns empty list when no sessions"
      (with-clean-engine
        (is (= {:ok []} (core/list-sessions)))))

    (testing "returns all active session IDs"
      (with-clean-engine
        (core/register! ::list-sess simple-chart)
        (let [{s1 :ok} (core/start! ::list-sess)
              {s2 :ok} (core/start! ::list-sess)
              {:keys [ok]} (core/list-sessions)]
          (is (= 2 (count ok)))
          (is (some #{s1} ok))
          (is (some #{s2} ok)))))))

(deftest list-charts-test
  (testing "list-charts"
    (testing "returns empty list when no charts registered"
      (with-clean-engine
        (is (= {:ok []} (core/list-charts)))))

    (testing "returns all registered chart names"
      (with-clean-engine
        (core/register! ::chart-a simple-chart)
        (core/register! ::chart-b simple-chart)
        (let [{:keys [ok]} (core/list-charts)]
          (is (= 2 (count ok)))
          (is (some #{::chart-a} ok))
          (is (some #{::chart-b} ok)))))))

(deftest available-events-test
  (testing "available-events"
    (testing "returns events available from current state"
      (with-clean-engine
        (core/register! ::avail-chart simple-chart)
        (let [{:keys [ok]} (core/start! ::avail-chart)
              session-id   ok
              result       (core/available-events session-id)]
          (is (contains? result :ok))
          (is (= #{:toggle} (:ok result))))))

    (testing "returns events after state change"
      (with-clean-engine
        (core/register! ::avail-trans simple-chart)
        (let [{:keys [ok]} (core/start! ::avail-trans)
              session-id   ok
              _            (core/send! session-id :toggle)
              result       (core/available-events session-id)]
          (is (= #{:toggle} (:ok result))))))

    (testing "returns error when session not found"
      (with-clean-engine
        (is (= {:error :session-not-found}
               (core/available-events "missing")))))))
