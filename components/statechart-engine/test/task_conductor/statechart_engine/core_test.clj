(ns task-conductor.statechart-engine.core-test
  ;; Test chart registration, session lifecycle, and event processing.
  ;; Uses a simple two-state chart: :off -> :on via :toggle event.
  (:require
   [clojure.test :refer [deftest is testing]]
   [com.fulcrologic.statecharts.elements :refer [state transition]]
   [com.fulcrologic.statecharts.chart :refer [statechart]]
   [task-conductor.statechart-engine.core :as core]
   [task-conductor.statechart-engine.test-helpers :refer [with-clean-engine]]))

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
        (is (= ::test-chart
               (core/register! ::test-chart simple-chart)))))

    (testing "throws when chart name already registered"
      (with-clean-engine
        (core/register! ::duplicate simple-chart)
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"already registered"
                              (core/register! ::duplicate simple-chart)))))

    (testing "throws when chart-def is nil"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid chart"
                              (core/register! ::nil-chart nil)))))

    (testing "throws when chart-def is not a map"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid chart"
                              (core/register! ::string-chart "not a chart")))))

    (testing "throws when chart-def has wrong :node-type"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Invalid chart"
                              (core/register! ::state-chart (state {:id :foo}))))))))

(deftest unregister!-test
  (testing "unregister!"
    (testing "removes a registered chart"
      (with-clean-engine
        (core/register! ::to-remove simple-chart)
        (is (= ::to-remove
               (core/unregister! ::to-remove)))))

    (testing "allows re-registration after unregistration"
      (with-clean-engine
        (core/register! ::reregister simple-chart)
        (core/unregister! ::reregister)
        (is (= ::reregister
               (core/register! ::reregister simple-chart)))))

    (testing "throws when chart not found"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (core/unregister! ::nonexistent)))))))

(deftest start!-test
  (testing "start!"
    (testing "starts a session for registered chart"
      (with-clean-engine
        (core/register! ::startable simple-chart)
        (let [session-id (core/start! ::startable)]
          (is (string? session-id))
          (is (uuid? (parse-uuid session-id))))))

    (testing "throws when chart not registered"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (core/start! ::not-registered)))))))

(deftest stop!-test
  (testing "stop!"
    (testing "stops an active session"
      (with-clean-engine
        (core/register! ::stoppable simple-chart)
        (let [session-id (core/start! ::stoppable)]
          (is (= session-id
                 (core/stop! session-id))))))

    (testing "throws when session not found"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (core/stop! "nonexistent-session")))))))

(deftest send!-test
  (testing "send!"
    (testing "transitions state on valid event"
      (with-clean-engine
        (core/register! ::toggle-chart simple-chart)
        (let [session-id (core/start! ::toggle-chart)
              state      (core/send! session-id :toggle)]
          (is (set? state))
          (is (contains? state :on)))))

    (testing "toggles back to original state"
      (with-clean-engine
        (core/register! ::toggle-back simple-chart)
        (let [session-id (core/start! ::toggle-back)
              _          (core/send! session-id :toggle)
              state      (core/send! session-id :toggle)]
          (is (contains? state :off)))))

    (testing "throws when session not found"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (core/send! "nonexistent" :toggle)))))))

;;; Introspection Tests

(deftest state-test
  (testing "state"
    (testing "returns current configuration for active session"
      (with-clean-engine
        (core/register! ::state-chart simple-chart)
        (let [session-id (core/start! ::state-chart)
              state      (core/state session-id)]
          (is (set? state))
          (is (contains? state :off)))))

    (testing "reflects state after transition"
      (with-clean-engine
        (core/register! ::state-trans simple-chart)
        (let [session-id (core/start! ::state-trans)
              _          (core/send! session-id :toggle)
              state      (core/state session-id)]
          (is (contains? state :on)))))

    (testing "throws when session not found"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (core/state "no-such-session")))))))

(deftest list-sessions-test
  (testing "list-sessions"
    (testing "returns empty list when no sessions"
      (with-clean-engine
        (is (= [] (core/list-sessions)))))

    (testing "returns all active session IDs"
      (with-clean-engine
        (core/register! ::list-sess simple-chart)
        (let [s1       (core/start! ::list-sess)
              s2       (core/start! ::list-sess)
              sessions (core/list-sessions)]
          (is (= 2 (count sessions)))
          (is (some #{s1} sessions))
          (is (some #{s2} sessions)))))))

(deftest list-charts-test
  (testing "list-charts"
    (testing "returns empty list when no charts registered"
      (with-clean-engine
        (is (= [] (core/list-charts)))))

    (testing "returns all registered chart names"
      (with-clean-engine
        (core/register! ::chart-a simple-chart)
        (core/register! ::chart-b simple-chart)
        (let [charts (core/list-charts)]
          (is (= 2 (count charts)))
          (is (some #{::chart-a} charts))
          (is (some #{::chart-b} charts)))))))

(deftest available-events-test
  (testing "available-events"
    (testing "returns events available from current state"
      (with-clean-engine
        (core/register! ::avail-chart simple-chart)
        (let [session-id (core/start! ::avail-chart)
              events     (core/available-events session-id)]
          (is (set? events))
          (is (= #{:toggle} events)))))

    (testing "returns events after state change"
      (with-clean-engine
        (core/register! ::avail-trans simple-chart)
        (let [session-id (core/start! ::avail-trans)
              _          (core/send! session-id :toggle)
              events     (core/available-events session-id)]
          (is (= #{:toggle} events)))))

    (testing "throws when session not found"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (core/available-events "missing")))))))

(deftest history-test
  ;; Tests history tracking: initial state recording, transition tracking, and cleanup.
  (testing "history"
    (testing "records initial state on session start"
      (with-clean-engine
        (core/register! ::hist-init simple-chart)
        (let [session-id (core/start! ::hist-init)
              entries    (core/history session-id)]
          (is (= 1 (count entries)))
          (let [entry (first entries)]
            (is (contains? (:state entry) :off))
            (is (nil? (:event entry)))
            (is (inst? (:timestamp entry)))))))

    (testing "appends entry after each transition"
      (with-clean-engine
        (core/register! ::hist-trans simple-chart)
        (let [session-id (core/start! ::hist-trans)
              _          (core/send! session-id :toggle)
              _          (core/send! session-id :toggle)
              entries    (core/history session-id)]
          (is (= 3 (count entries)))
          (let [[e1 e2 e3] entries]
            (is (contains? (:state e1) :off))
            (is (nil? (:event e1)))
            (is (contains? (:state e2) :on))
            (is (= :toggle (:event e2)))
            (is (contains? (:state e3) :off))
            (is (= :toggle (:event e3)))))))

    (testing "returns last n entries with limit parameter"
      (with-clean-engine
        (core/register! ::hist-limit simple-chart)
        (let [session-id (core/start! ::hist-limit)
              _          (core/send! session-id :toggle)
              _          (core/send! session-id :toggle)
              _          (core/send! session-id :toggle)
              entries    (core/history session-id 2)]
          (is (= 2 (count entries)))
          (let [[e1 e2] entries]
            (is (= :toggle (:event e1)))
            (is (= :toggle (:event e2)))))))

    (testing "clears history when session stopped"
      (with-clean-engine
        (core/register! ::hist-stop simple-chart)
        (let [session-id (core/start! ::hist-stop)
              _          (core/send! session-id :toggle)
              _          (core/stop! session-id)]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                                (core/history session-id))))))

    (testing "throws when session not found"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (core/history "no-such-session")))))

    (testing "respects max-history-size option"
      (with-clean-engine
        (core/register! ::hist-limited simple-chart)
        (let [session-id (core/start! ::hist-limited {:max-history-size 3})
              _          (core/send! session-id :toggle)
              _          (core/send! session-id :toggle)
              _          (core/send! session-id :toggle)
              _          (core/send! session-id :toggle)
              entries    (core/history session-id)]
          (is (= 3 (count entries)))
          (let [[e1 e2 e3] entries]
            (is (= :toggle (:event e1)))
            (is (= :toggle (:event e2)))
            (is (= :toggle (:event e3)))))))

    (testing "unlimited history when no max-history-size"
      (with-clean-engine
        (core/register! ::hist-unlimited simple-chart)
        (let [session-id (core/start! ::hist-unlimited)]
          (dotimes [_ 10] (core/send! session-id :toggle))
          (let [entries (core/history session-id)]
            (is (= 11 (count entries)))))))))

;;; Session Data Tests

(deftest get-data-test
  ;; Tests session data storage and retrieval.
  (testing "get-data"
    (testing "returns empty map when no initial data provided"
      (with-clean-engine
        (core/register! ::data-empty simple-chart)
        (let [session-id (core/start! ::data-empty)
              data (core/get-data session-id)]
          ;; Only session-id is auto-added
          (is (= {:session-id session-id} data)))))

    (testing "returns initial data when provided"
      (with-clean-engine
        (core/register! ::data-init simple-chart)
        (let [session-id (core/start! ::data-init {:data {:foo "bar" :count 42}})
              data (core/get-data session-id)]
          (is (= "bar" (:foo data)))
          (is (= 42 (:count data)))
          (is (= session-id (:session-id data))))))

    (testing "throws when session not found"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (core/get-data "no-such-session")))))))

(deftest update-data!-test
  ;; Tests session data updates.
  (testing "update-data!"
    (testing "updates data using provided function"
      (with-clean-engine
        (core/register! ::data-update simple-chart)
        (let [session-id (core/start! ::data-update {:data {:count 0}})
              result (core/update-data! session-id #(update % :count inc))]
          (is (= 1 (:count result)))
          (is (= 1 (:count (core/get-data session-id)))))))

    (testing "adds new keys"
      (with-clean-engine
        (core/register! ::data-add simple-chart)
        (let [session-id (core/start! ::data-add {:data {:a 1}})
              result (core/update-data! session-id #(assoc % :b 2))]
          (is (= 1 (:a result)))
          (is (= 2 (:b result))))))

    (testing "throws when session not found"
      (with-clean-engine
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                              (core/update-data! "no-such-session" identity)))))))

(deftest session-data-cleanup-test
  ;; Tests that session data is cleaned up when session stops.
  (testing "session data cleanup"
    (testing "clears data when session stopped"
      (with-clean-engine
        (core/register! ::data-stop simple-chart)
        (let [session-id (core/start! ::data-stop {:data {:keep "me"}})]
          (is (= "me" (:keep (core/get-data session-id))))
          (core/stop! session-id)
          (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                                (core/get-data session-id))))))))
