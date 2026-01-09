(ns task-conductor.agent-runner.repl-test
  ;; Tests the REPL control functions for story execution.
  ;; Verifies:
  ;; - start-story validates :idle state and transitions to :selecting-task
  ;; - status returns correct map and prints summary
  ;; - pause/continue set/clear the paused flag
  ;; - abort transitions through :error-recovery to :idle
  ;; - retry re-attempts failed task from :error-recovery
  ;; - skip moves to next task from :error-recovery
  ;; - add-context validates story and calls mcp-tasks CLI
  ;; - view-context retrieves and formats shared-context
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

    (testing "returns current state map"
      (console/reset-state!)
      (let [result (repl/pause)]
        (is (map? result))
        (is (true? (:paused result)))))

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

    (testing "returns current state map"
      (console/reset-state!)
      (console/set-paused!)
      (let [result (repl/continue)]
        (is (map? result))
        (is (false? (:paused result)))))

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

;;; retry Tests

(defn- setup-error-recovery-state
  "Helper to set up a state machine in :error-recovery with a task."
  [story-id session-id task-id]
  (console/reset-state!)
  (console/transition! :selecting-task {:story-id story-id})
  (console/transition! :running-sdk {:session-id session-id
                                     :current-task-id task-id})
  (console/transition! :error-recovery {:error {:type :test-error}}))

(deftest retry-test
  (testing "retry"
    (testing "from :error-recovery state"
      (testing "transitions to :running-sdk"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/retry)
        (is (= :running-sdk (console/current-state))))

      (testing "preserves task-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/retry)
        (is (= 75 (:current-task-id @console/console-state))))

      (testing "preserves session-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/retry)
        (is (= "sess-1" (:session-id @console/console-state))))

      (testing "returns new state map"
        (setup-error-recovery-state 53 "sess-1" 75)
        (let [result (repl/retry)]
          (is (map? result))
          (is (= :running-sdk (:state result)))
          (is (= 75 (:current-task-id result)))))

      (testing "prints confirmation with task-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (let [output (with-out-str (repl/retry))]
          (is (re-find #"Retrying task 75" output)))))

    (testing "from non-error-recovery state"
      (testing "throws with informative error"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [ex (try
                   (repl/retry)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :invalid-state (:type (ex-data ex))))
          (is (= :selecting-task (:current-state (ex-data ex))))
          (is (= :error-recovery (:required-state (ex-data ex)))))))))

;;; skip Tests

(deftest skip-test
  (testing "skip"
    (testing "from :error-recovery state"
      (testing "transitions to :selecting-task"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/skip)
        (is (= :selecting-task (console/current-state))))

      (testing "preserves current-task-id"
        ;; task-id is preserved in state; it will be overwritten when
        ;; the outer loop selects and starts the next task
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/skip)
        (is (= 75 (:current-task-id @console/console-state))))

      (testing "preserves story-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (repl/skip)
        (is (= 53 (:story-id @console/console-state))))

      (testing "returns new state map"
        (setup-error-recovery-state 53 "sess-1" 75)
        (let [result (repl/skip)]
          (is (map? result))
          (is (= :selecting-task (:state result)))))

      (testing "prints confirmation with task-id"
        (setup-error-recovery-state 53 "sess-1" 75)
        (let [output (with-out-str (repl/skip))]
          (is (re-find #"Skipped task 75" output)))))

    (testing "from non-error-recovery state"
      (testing "throws with informative error"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [ex (try
                   (repl/skip)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :invalid-state (:type (ex-data ex))))
          (is (= :selecting-task (:current-state (ex-data ex))))
          (is (= :error-recovery (:required-state (ex-data ex)))))))))

;;; Context Management Tests

(deftest add-context-test
  (testing "add-context"
    (testing "with no active story"
      (testing "throws with informative error"
        (console/reset-state!)
        (let [ex (try
                   (repl/add-context "test context")
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :no-active-story (:type (ex-data ex))))
          (is (= :idle (:current-state (ex-data ex)))))))

    (testing "with active story"
      (testing "uses story-id from console state"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        ;; Can't fully test without mocking CLI, but validates story-id extraction
        ;; The CLI call will fail in tests, which is expected
        (let [ex (try
                   (repl/add-context "test context")
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          ;; If we get a CLI error (not no-active-story), the story-id was validated
          (when ex
            (is (not= :no-active-story (:type (ex-data ex))))))))))

(deftest view-context-test
  (testing "view-context"
    (testing "with no active story"
      (testing "throws with informative error"
        (console/reset-state!)
        (let [ex (try
                   (repl/view-context)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :no-active-story (:type (ex-data ex))))
          (is (= :idle (:current-state (ex-data ex)))))))

    (testing "with active story"
      (testing "uses story-id from console state"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        ;; Can't fully test without mocking CLI, but validates story-id extraction
        (let [ex (try
                   (repl/view-context)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          ;; If we get a CLI error (not no-active-story), the story-id was validated
          (when ex
            (is (not= :no-active-story (:type (ex-data ex))))))))))

;;; Session Tracking Tests

(deftest list-sessions-test
  (testing "list-sessions"
    (testing "with no active story"
      (testing "throws with informative error"
        (console/reset-state!)
        (let [ex (try
                   (repl/list-sessions)
                   nil
                   (catch clojure.lang.ExceptionInfo e e))]
          (is (some? ex))
          (is (= :no-active-story (:type (ex-data ex))))
          (is (= :idle (:current-state (ex-data ex)))))))

    (testing "with active story"
      (testing "returns empty vector when no sessions"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [result (repl/list-sessions)]
          (is (vector? result))
          (is (empty? result))))

      (testing "returns sessions from console state"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/record-session! "sess-1" 75)
        (console/record-session! "sess-2" 76)
        (let [result (repl/list-sessions)]
          (is (= 2 (count result)))
          (is (= "sess-1" (:session-id (first result))))
          (is (= "sess-2" (:session-id (second result))))))

      (testing "prints empty message when no sessions"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (let [output (with-out-str (repl/list-sessions))]
          (is (re-find #"Sessions for story 53:" output))
          (is (re-find #"\(none\)" output))))

      (testing "prints formatted session list"
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/record-session! "sess-abc" 75)
        (let [output (with-out-str (repl/list-sessions))]
          (is (re-find #"Sessions for story 53:" output))
          (is (re-find #"sess-abc" output))
          (is (re-find #"task 75" output)))))))
