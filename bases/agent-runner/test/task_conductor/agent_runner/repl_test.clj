(ns task-conductor.agent-runner.repl-test
  ;; Tests the REPL control functions for story execution.
  ;; Verifies:
  ;; - start-story validates :idle state and transitions to :selecting-task
  ;; - status returns correct map and prints summary
  ;; - pause/continue set/clear the paused flag
  ;; - abort transitions through :error-recovery to :idle
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.console :as console]
   [task-conductor.agent-runner.repl :as repl]))

;;; start-story Tests

(deftest start-story-test
  (testing "start-story"
    (testing "from :idle state"
      (testing "transitions to :selecting-task"
        (console/reset-state!)
        (repl/start-story 53)
        (is (= :selecting-task (console/current-state))))

      (testing "sets story-id"
        (console/reset-state!)
        (repl/start-story 53)
        (is (= 53 (:story-id @console/console-state))))

      (testing "returns new state map"
        (console/reset-state!)
        (let [result (repl/start-story 53)]
          (is (map? result))
          (is (= :selecting-task (:state result)))
          (is (= 53 (:story-id result)))))

      (testing "prints confirmation"
        (console/reset-state!)
        (let [output (with-out-str (repl/start-story 53))]
          (is (re-find #"Started story 53" output)))))

    (testing "from non-idle state"
      (testing "throws with informative error"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 42})
        (let [ex (try
                   (repl/start-story 53)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :invalid-state (:type (ex-data ex))))
          (is (= :selecting-task (:current-state (ex-data ex))))
          (is (= :idle (:required-state (ex-data ex)))))))))

;;; status Tests

(deftest status-test
  (testing "status"
    (testing "returns map with required keys"
      (console/reset-state!)
      (let [result (repl/status)]
        (is (contains? result :state))
        (is (contains? result :story-id))
        (is (contains? result :current-task-id))
        (is (contains? result :paused))))

    (testing "returns correct values for :idle state"
      (console/reset-state!)
      (let [result (repl/status)]
        (is (= :idle (:state result)))
        (is (nil? (:story-id result)))
        (is (nil? (:current-task-id result)))
        (is (false? (:paused result)))))

    (testing "returns correct values for :running-sdk state"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "sess-1"
                                         :current-task-id 75})
      (let [result (repl/status)]
        (is (= :running-sdk (:state result)))
        (is (= 53 (:story-id result)))
        (is (= 75 (:current-task-id result)))))

    (testing "reflects paused state"
      (console/reset-state!)
      (console/set-paused!)
      (let [result (repl/status)]
        (is (true? (:paused result)))))

    (testing "prints state"
      (console/reset-state!)
      (let [output (with-out-str (repl/status))]
        (is (re-find #"State: :idle" output))))

    (testing "prints story-id when present"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (let [output (with-out-str (repl/status))]
        (is (re-find #"Story: 53" output))))

    (testing "prints task-id when present"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "s1"
                                         :current-task-id 75})
      (let [output (with-out-str (repl/status))]
        (is (re-find #"Task: 75" output))))

    (testing "prints PAUSED when paused"
      (console/reset-state!)
      (console/set-paused!)
      (let [output (with-out-str (repl/status))]
        (is (re-find #"PAUSED" output))))))

;;; pause Tests

(deftest pause-test
  (testing "pause"
    (testing "sets paused flag to true"
      (console/reset-state!)
      (repl/pause)
      (is (true? (console/paused?))))

    (testing "returns true"
      (console/reset-state!)
      (is (true? (repl/pause))))

    (testing "prints confirmation"
      (console/reset-state!)
      (let [output (with-out-str (repl/pause))]
        (is (re-find #"Paused" output))))))

;;; continue Tests

(deftest continue-test
  (testing "continue"
    (testing "clears paused flag"
      (console/reset-state!)
      (console/set-paused!)
      (repl/continue)
      (is (false? (console/paused?))))

    (testing "returns false"
      (console/reset-state!)
      (console/set-paused!)
      (is (false? (repl/continue))))

    (testing "prints confirmation"
      (console/reset-state!)
      (console/set-paused!)
      (let [output (with-out-str (repl/continue))]
        (is (re-find #"Resumed" output))))))

;;; abort Tests

(deftest abort-test
  (testing "abort"
    (testing "from :idle state"
      (testing "returns current state"
        (console/reset-state!)
        (let [result (repl/abort)]
          (is (= :idle (:state result)))))

      (testing "prints already idle message"
        (console/reset-state!)
        (let [output (with-out-str (repl/abort))]
          (is (re-find #"Already idle" output)))))

    (testing "from :story-complete state"
      (testing "transitions directly to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :story-complete)
        (repl/abort)
        (is (= :idle (console/current-state))))

      (testing "prints aborted message"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :story-complete)
        (let [output (with-out-str (repl/abort))]
          (is (re-find #"Aborted" output)))))

    (testing "from :selecting-task state"
      (testing "transitions through :error-recovery to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (repl/abort)
        (is (= :idle (console/current-state))))

      (testing "records :user-abort error"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (repl/abort)
        (let [history (console/state-history)
              error-entry (first (filter #(= :error-recovery (:to %)) history))]
          (is (some? error-entry))
          (is (= :user-abort (get-in error-entry [:context :error :type]))))))

    (testing "from :running-sdk state"
      (testing "transitions to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "s1"
                                           :current-task-id 75})
        (repl/abort)
        (is (= :idle (console/current-state)))))

    (testing "from :needs-input state"
      (testing "transitions to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "s1"
                                           :current-task-id 75})
        (console/transition! :needs-input)
        (repl/abort)
        (is (= :idle (console/current-state)))))

    (testing "from :task-complete state"
      (testing "transitions to :idle"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "s1"
                                           :current-task-id 75})
        (console/transition! :task-complete)
        (repl/abort)
        (is (= :idle (console/current-state)))))

    (testing "preserves history"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "s1"
                                         :current-task-id 75})
      (repl/abort)
      (let [history (console/state-history)]
        (is (>= (count history) 4))))))
