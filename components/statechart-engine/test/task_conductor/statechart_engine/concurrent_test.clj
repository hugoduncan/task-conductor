(ns task-conductor.statechart-engine.concurrent-test
  ;; Verify thread-safety when multiple sessions are accessed concurrently.
  ;; Tests concurrent data updates, event sends, and session lifecycle operations.
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

(def counter-chart
  "Single-state chart with :inc and :dec events for testing data updates."
  (statechart {}
              (state {:id :idle}
                     (transition {:event :inc :target :idle})
                     (transition {:event :dec :target :idle}))))

;;; Concurrent Session Data Tests

(deftest concurrent-session-data-updates-test
  ;; Verify multiple sessions can update their session-data simultaneously
  ;; without data corruption or cross-session interference.
  (testing "concurrent session data updates"
    (testing "multiple sessions updating data in parallel"
      (with-clean-engine
        (core/register! ::concurrent-data counter-chart)
        (let [session-ids (doall
                           (for [i (range 5)]
                             (core/start! ::concurrent-data {:data {:id i :count 0}})))
              ;; Run concurrent updates
              futures (doall
                       (for [session-id session-ids]
                         (future
                           (dotimes [_ 100]
                             (core/update-data! session-id
                                                #(update % :count inc))))))
              ;; Wait for all to complete
              _ (doseq [f futures] @f)]
          ;; Verify each session has the correct count
          (doseq [session-id session-ids]
            (let [data (core/get-data session-id)]
              (is (= 100 (:count data))
                  (str "Session " session-id " should have count 100")))))))

    (testing "concurrent updates to same session"
      (with-clean-engine
        (core/register! ::single-session counter-chart)
        (let [session-id (core/start! ::single-session {:data {:count 0}})
              ;; Run concurrent updates from multiple threads
              futures (doall
                       (for [_ (range 10)]
                         (future
                           (dotimes [_ 100]
                             (core/update-data! session-id
                                                #(update % :count inc))))))]
          ;; Wait for all to complete
          (doseq [f futures] @f)
          ;; Verify final count (10 threads * 100 increments)
          (is (= 1000 (:count (core/get-data session-id)))))))))

;;; Concurrent Event Send Tests

(deftest concurrent-send-test
  ;; Verify concurrent send! calls to different sessions work correctly.
  (testing "concurrent send! calls"
    (testing "to different sessions in parallel"
      (with-clean-engine
        (core/register! ::parallel-send simple-chart)
        (let [session-ids (doall (for [_ (range 10)]
                                   (core/start! ::parallel-send)))
              ;; Send toggle events concurrently
              futures (doall
                       (for [session-id session-ids]
                         (future
                           (dotimes [_ 50]
                             (core/send! session-id :toggle))
                           ;; Return final state
                           (core/state session-id))))]
          ;; Wait for all and verify states
          (doseq [[session-id f] (map vector session-ids futures)]
            (let [final-state @f]
              ;; 50 toggles means should be in :off state (even count)
              (is (contains? final-state :off)
                  (str "Session " session-id " should be in :off state")))))))

    (testing "rapid events to same session"
      (with-clean-engine
        (core/register! ::rapid-events simple-chart)
        (let [session-id (core/start! ::rapid-events)
              ;; Send many events concurrently from multiple threads
              futures (doall
                       (for [_ (range 5)]
                         (future
                           (dotimes [_ 20]
                             (core/send! session-id :toggle)))))]
          ;; Wait for all
          (doseq [f futures] @f)
          ;; 5 threads * 20 toggles = 100 toggles (even) -> :off
          (is (contains? (core/state session-id) :off)))))))

;;; Concurrent Session Lifecycle Tests

(deftest concurrent-session-lifecycle-test
  ;; Verify session start/stop operations are thread-safe.
  (testing "concurrent session lifecycle"
    (testing "starting sessions while others are active"
      (with-clean-engine
        (core/register! ::lifecycle simple-chart)
        (let [;; Start initial sessions
              initial-sessions (doall (for [_ (range 5)]
                                        (core/start! ::lifecycle)))
              ;; Start more sessions concurrently while sending to initial ones
              start-futures (doall
                             (for [_ (range 5)]
                               (future (core/start! ::lifecycle))))
              send-futures (doall
                            (for [s initial-sessions]
                              (future
                                (dotimes [_ 10]
                                  (core/send! s :toggle))
                                s)))]
          ;; Collect new sessions
          (let [new-sessions (mapv deref start-futures)]
            ;; All sessions should be valid
            (doseq [s (concat initial-sessions new-sessions)]
              (is (set? (core/state s))
                  (str "Session " s " should have valid state"))))
          ;; Wait for sends
          (doseq [f send-futures] @f)
          ;; All 10 sessions should be listed
          (is (= 10 (count (core/list-sessions)))))))

    (testing "stopping sessions while others continue"
      (with-clean-engine
        (core/register! ::stop-while-active simple-chart)
        (let [sessions (doall (for [_ (range 10)]
                                (core/start! ::stop-while-active)))
              [to-stop to-keep] (split-at 5 sessions)
              ;; Concurrently stop some while sending to others
              stop-futures (doall (for [s to-stop]
                                    (future (core/stop! s))))
              send-futures (doall (for [s to-keep]
                                    (future
                                      (dotimes [_ 20]
                                        (core/send! s :toggle))
                                      (core/state s))))]
          ;; Wait for stops
          (doseq [f stop-futures] @f)
          ;; Verify stopped sessions are gone
          (doseq [s to-stop]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                                  (core/state s))))
          ;; Verify kept sessions still work
          (doseq [f send-futures]
            (let [final-state @f]
              (is (contains? final-state :off)
                  "Active sessions should complete successfully")))
          ;; Verify only 5 sessions remain
          (is (= 5 (count (core/list-sessions)))))))

    (testing "rapid start/stop cycles"
      (with-clean-engine
        (core/register! ::rapid-lifecycle simple-chart)
        (let [;; Run rapid start/stop cycles in parallel
              futures (doall
                       (for [_ (range 10)]
                         (future
                           (dotimes [_ 50]
                             (let [s (core/start! ::rapid-lifecycle)]
                               (core/send! s :toggle)
                               (core/stop! s))))))
              ;; Also have some long-running sessions
              long-session (core/start! ::rapid-lifecycle)]
          ;; Wait for all cycles
          (doseq [f futures] @f)
          ;; Long-running session should still work
          (is (contains? (core/state long-session) :off))
          (core/send! long-session :toggle)
          (is (contains? (core/state long-session) :on)))))))

;;; Cross-Session Data Isolation Tests

(deftest session-data-isolation-test
  ;; Verify that concurrent operations don't cause data leakage between sessions.
  (testing "session data isolation"
    (testing "under concurrent access"
      (with-clean-engine
        (core/register! ::isolation counter-chart)
        (let [;; Create sessions with distinct identifiers
              sessions (doall
                        (for [i (range 5)]
                          {:id i
                           :session-id (core/start! ::isolation
                                                    {:data {:owner i :value 0}})}))
              ;; Concurrently update each session's value
              futures (doall
                       (for [{:keys [id session-id]} sessions]
                         (future
                           (dotimes [_ 100]
                             (core/update-data!
                              session-id
                              (fn [data]
                                ;; Include owner check
                                (assert (= id (:owner data))
                                        "Data should belong to correct session")
                                (update data :value inc))))
                           {:id id
                            :final (core/get-data session-id)})))]
          ;; Verify each session has isolated data
          (doseq [f futures]
            (let [{:keys [id final]} @f]
              (is (= id (:owner final))
                  "Owner should be preserved")
              (is (= 100 (:value final))
                  "Value should reflect only this session's updates"))))))))

;;; History Thread-Safety Tests

(deftest concurrent-history-test
  ;; Verify history tracking is thread-safe under concurrent event sends.
  (testing "concurrent history access"
    (testing "history remains consistent under concurrent sends"
      (with-clean-engine
        (core/register! ::history-concurrent simple-chart)
        (let [session-id (core/start! ::history-concurrent)
              ;; Send events from multiple threads
              futures (doall
                       (for [_ (range 5)]
                         (future
                           (dotimes [_ 20]
                             (core/send! session-id :toggle)))))]
          ;; Wait for all sends
          (doseq [f futures] @f)
          ;; History should have initial + 100 events = 101 entries
          (let [history (core/history session-id)]
            (is (= 101 (count history)))
            ;; First entry should have nil event (initial)
            (is (nil? (:event (first history))))
            ;; All subsequent should have :toggle
            (is (every? #(= :toggle (:event %)) (rest history)))))))))
