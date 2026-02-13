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
  "Single-state chart with :inc event for testing data updates."
  (statechart {}
              (state {:id :idle}
                     (transition {:event :inc :target :idle}))))

;;; Concurrent Session Data Tests

(deftest concurrent-session-data-updates-test
  ;; Verify multiple sessions can update their session-data simultaneously
  ;; without data corruption or cross-session interference.
  (testing "concurrent session data updates"
    (testing "multiple sessions updating data in parallel"
      (with-clean-engine
        (core/register! ::concurrent-data counter-chart)
        (let [session-ids (doall
                           (for [i (range 3)]
                             (core/start!
                              ::concurrent-data
                              {:data {:id i :count 0}})))
              futures (doall
                       (for [session-id session-ids]
                         (future
                           (dotimes [_ 20]
                             (core/update-data!
                              session-id
                              #(update % :count inc))))))
              _ (doseq [f futures] @f)]
          (doseq [session-id session-ids]
            (let [data (core/get-data session-id)]
              (is (= 20 (:count data))
                  (str "Session " session-id " should have count 20")))))))

    (testing "concurrent updates to same session"
      (with-clean-engine
        (core/register! ::single-session counter-chart)
        (let [session-id (core/start! ::single-session {:data {:count 0}})
              futures (doall
                       (for [_ (range 5)]
                         (future
                           (dotimes [_ 20]
                             (core/update-data!
                              session-id
                              #(update % :count inc))))))]
          (doseq [f futures] @f)
          (is (= 100 (:count (core/get-data session-id)))))))))

;;; Concurrent Event Send Tests

(deftest concurrent-send-test
  ;; Verify concurrent send! calls to different sessions work correctly.
  (testing "concurrent send! calls"
    (testing "to different sessions in parallel"
      (with-clean-engine
        (core/register! ::parallel-send simple-chart)
        (let [session-ids (doall (for [_ (range 5)]
                                   (core/start! ::parallel-send)))
              futures (doall
                       (for [session-id session-ids]
                         (future
                           (dotimes [_ 10]
                             (core/send! session-id :toggle))
                           (core/state session-id))))]
          (doseq [[session-id f] (map vector session-ids futures)]
            (let [final-state @f]
              ;; 10 toggles -> :off (even count)
              (is (contains? final-state :off)
                  (str "Session " session-id " should be in :off state")))))))

    (testing "rapid events to same session"
      (with-clean-engine
        (core/register! ::rapid-events simple-chart)
        (let [session-id (core/start! ::rapid-events)
              futures (doall
                       (for [_ (range 4)]
                         (future
                           (dotimes [_ 10]
                             (core/send! session-id :toggle)))))]
          (doseq [f futures] @f)
          ;; 40 toggles (even) -> :off
          (is (contains? (core/state session-id) :off)))))))

;;; Concurrent Session Lifecycle Tests

(deftest concurrent-session-lifecycle-test
  ;; Verify session start/stop operations are thread-safe.
  (testing "concurrent session lifecycle"
    (testing "starting sessions while others are active"
      (with-clean-engine
        (core/register! ::lifecycle simple-chart)
        (let [initial-sessions (doall (for [_ (range 3)]
                                        (core/start! ::lifecycle)))
              start-futures (doall
                             (for [_ (range 3)]
                               (future (core/start! ::lifecycle))))
              send-futures (doall
                            (for [s initial-sessions]
                              (future
                                (dotimes [_ 4]
                                  (core/send! s :toggle))
                                s)))]
          (let [new-sessions (mapv deref start-futures)]
            (doseq [s (concat initial-sessions new-sessions)]
              (is (set? (core/state s))
                  (str "Session " s " should have valid state"))))
          (doseq [f send-futures] @f)
          (is (= 6 (count (core/list-sessions)))))))

    (testing "stopping sessions while others continue"
      (with-clean-engine
        (core/register! ::stop-while-active simple-chart)
        (let [sessions (doall (for [_ (range 6)]
                                (core/start! ::stop-while-active)))
              [to-stop to-keep] (split-at 3 sessions)
              stop-futures (doall (for [s to-stop]
                                    (future (core/stop! s))))
              send-futures (doall (for [s to-keep]
                                    (future
                                      (dotimes [_ 10]
                                        (core/send! s :toggle))
                                      (core/state s))))]
          (doseq [f stop-futures] @f)
          (doseq [s to-stop]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo #"not found"
                                  (core/state s))))
          (doseq [f send-futures]
            (let [final-state @f]
              (is (contains? final-state :off)
                  "Active sessions should complete successfully")))
          (is (= 3 (count (core/list-sessions)))))))

    (testing "rapid start/stop cycles"
      (with-clean-engine
        (core/register! ::rapid-lifecycle simple-chart)
        (let [futures (doall
                       (for [_ (range 4)]
                         (future
                           (dotimes [_ 10]
                             (let [s (core/start! ::rapid-lifecycle)]
                               (core/send! s :toggle)
                               (core/stop! s))))))
              long-session (core/start! ::rapid-lifecycle)]
          (doseq [f futures] @f)
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
        (let [sessions (doall
                        (for [i (range 3)]
                          {:id i
                           :session-id (core/start! ::isolation
                                                    {:data {:owner i
                                                            :value 0}})}))
              futures (doall
                       (for [{:keys [id session-id]} sessions]
                         (future
                           (dotimes [_ 20]
                             (core/update-data!
                              session-id
                              (fn [data]
                                (assert (= id (:owner data))
                                        "Data should belong to correct session")
                                (update data :value inc))))
                           {:id id
                            :final (core/get-data session-id)})))]
          (doseq [f futures]
            (let [{:keys [id final]} @f]
              (is (= id (:owner final)) "Owner should be preserved")
              (is (= 20 (:value final))
                  "Value should reflect only this session's updates"))))))))

;;; History Thread-Safety Tests

(deftest concurrent-history-test
  ;; Verify history tracking is thread-safe under concurrent event sends.
  (testing "concurrent history access"
    (testing "history remains consistent under concurrent sends"
      (with-clean-engine
        (core/register! ::history-concurrent simple-chart)
        (let [session-id (core/start! ::history-concurrent)
              futures (doall
                       (for [_ (range 4)]
                         (future
                           (dotimes [_ 10]
                             (core/send! session-id :toggle)))))]
          (doseq [f futures] @f)
          ;; History should have initial + 40 events = 41 entries
          (let [history (core/history session-id)]
            (is (= 41 (count history)))
            (is (nil? (:event (first history))))
            (is (every? #(= :toggle (:event %)) (rest history)))))))))
