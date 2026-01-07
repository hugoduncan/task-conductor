(ns task-conductor.agent-runner.console-test
  ;; Tests the console state machine transition logic.
  ;; Verifies:
  ;; - valid-transitions map defines correct transition graph
  ;; - can-transition? correctly identifies valid/invalid transitions
  ;; - transition function applies context and validates transitions
  ;; - invalid transitions throw informative errors
  ;; - handoff file integration writes state on relevant transitions
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.console :as console]
   [task-conductor.agent-runner.handoff :as handoff])
  (:import
   [java.io File]))

(deftest valid-transitions-test
  (testing "valid-transitions"
    (testing "defines all expected states"
      (is (= #{:idle :selecting-task :running-sdk :needs-input
               :running-cli :error-recovery :task-complete :story-complete}
             console/all-states)
          "should include all 8 states"))

    (testing "from :idle"
      (is (= #{:selecting-task}
             (get console/valid-transitions :idle))
          "should only allow transition to :selecting-task"))

    (testing "from :selecting-task"
      (is (= #{:running-sdk :story-complete}
             (get console/valid-transitions :selecting-task))
          "should allow :running-sdk or :story-complete"))

    (testing "from :running-sdk"
      (is (= #{:needs-input :task-complete :error-recovery}
             (get console/valid-transitions :running-sdk))
          "should allow :needs-input, :task-complete, or :error-recovery"))

    (testing "from :needs-input"
      (is (= #{:running-cli}
             (get console/valid-transitions :needs-input))
          "should only allow transition to :running-cli"))

    (testing "from :running-cli"
      (is (= #{:running-sdk :error-recovery}
             (get console/valid-transitions :running-cli))
          "should allow :running-sdk or :error-recovery"))

    (testing "from :error-recovery"
      (is (= #{:selecting-task :idle}
             (get console/valid-transitions :error-recovery))
          "should allow :selecting-task or :idle"))

    (testing "from :task-complete"
      (is (= #{:selecting-task :story-complete}
             (get console/valid-transitions :task-complete))
          "should allow :selecting-task or :story-complete"))

    (testing "from :story-complete"
      (is (= #{:idle}
             (get console/valid-transitions :story-complete))
          "should only allow transition to :idle"))))

(deftest can-transition?-test
  (testing "can-transition?"
    (testing "with state keyword"
      (testing "returns true for valid transitions"
        (is (true? (console/can-transition? :idle :selecting-task)))
        (is (true? (console/can-transition? :selecting-task :running-sdk)))
        (is (true? (console/can-transition? :running-sdk :needs-input)))
        (is (true? (console/can-transition? :needs-input :running-cli)))
        (is (true? (console/can-transition? :running-cli :running-sdk))))

      (testing "returns false for invalid transitions"
        (is (false? (console/can-transition? :idle :running-sdk))
            ":idle cannot go directly to :running-sdk")
        (is (false? (console/can-transition? :idle :idle))
            "self-transition not allowed for :idle")
        (is (false? (console/can-transition? :running-sdk :idle))
            ":running-sdk cannot go directly to :idle")
        (is (false? (console/can-transition? :needs-input :task-complete))
            ":needs-input must go through :running-cli")))

    (testing "with state map"
      (testing "returns true for valid transitions"
        (is (true? (console/can-transition? {:state :idle} :selecting-task)))
        (is (true? (console/can-transition? {:state :running-sdk
                                             :session-id "abc"
                                             :current-task-id 42}
                                            :task-complete))))

      (testing "returns false for invalid transitions"
        (is (false? (console/can-transition? {:state :idle} :running-sdk)))
        (is (false? (console/can-transition? {:state :story-complete} :running-sdk)))))

    (testing "with unknown state"
      (testing "returns false"
        (is (false? (console/can-transition? :unknown :idle)))
        (is (false? (console/can-transition? :idle :unknown)))))))

(deftest transition-test
  (testing "transition"
    (testing "from :idle to :selecting-task"
      (let [initial {:state :idle
                     :story-id nil
                     :current-task-id nil
                     :session-id nil
                     :error nil}
            result (console/transition initial :selecting-task {:story-id 53})]
        (is (= :selecting-task (:state result))
            "should update state to :selecting-task")
        (is (= 53 (:story-id result))
            "should set story-id from context")
        (is (nil? (:error result))
            "should clear error")))

    (testing "from :selecting-task to :running-sdk"
      (let [state {:state :selecting-task
                   :story-id 53
                   :current-task-id nil
                   :session-id nil
                   :error nil}
            result (console/transition state :running-sdk
                                       {:session-id "sess-123"
                                        :current-task-id 75})]
        (is (= :running-sdk (:state result)))
        (is (= "sess-123" (:session-id result))
            "should set session-id from context")
        (is (= 75 (:current-task-id result))
            "should set current-task-id from context")
        (is (= 53 (:story-id result))
            "should preserve story-id")))

    (testing "from :selecting-task to :story-complete"
      (let [state {:state :selecting-task
                   :story-id 53
                   :current-task-id nil
                   :session-id "old-sess"
                   :error nil}
            result (console/transition state :story-complete)]
        (is (= :story-complete (:state result)))
        (is (nil? (:session-id result))
            "should clear session-id")
        (is (nil? (:current-task-id result))
            "should clear current-task-id")
        (is (= 53 (:story-id result))
            "should preserve story-id")))

    (testing "from :running-sdk to :needs-input"
      (let [state {:state :running-sdk
                   :story-id 53
                   :current-task-id 75
                   :session-id "sess-123"
                   :error nil}
            result (console/transition state :needs-input)]
        (is (= :needs-input (:state result)))
        (is (= "sess-123" (:session-id result))
            "should preserve session-id")
        (is (= 75 (:current-task-id result))
            "should preserve current-task-id")))

    (testing "from :running-sdk to :task-complete"
      (let [state {:state :running-sdk
                   :story-id 53
                   :current-task-id 75
                   :session-id "sess-123"
                   :error nil}
            result (console/transition state :task-complete)]
        (is (= :task-complete (:state result)))
        (is (nil? (:current-task-id result))
            "should clear current-task-id")
        (is (= "sess-123" (:session-id result))
            "should preserve session-id")))

    (testing "from :running-sdk to :error-recovery"
      (let [state {:state :running-sdk
                   :story-id 53
                   :current-task-id 75
                   :session-id "sess-123"
                   :error nil}
            error-info {:type :sdk-error :message "Connection lost"}
            result (console/transition state :error-recovery {:error error-info})]
        (is (= :error-recovery (:state result)))
        (is (= error-info (:error result))
            "should set error from context")
        (is (= 75 (:current-task-id result))
            "should preserve current-task-id for retry")))

    (testing "from :needs-input to :running-cli"
      (let [state {:state :needs-input
                   :story-id 53
                   :current-task-id 75
                   :session-id "sess-123"
                   :error nil}
            result (console/transition state :running-cli)]
        (is (= :running-cli (:state result)))
        (is (= "sess-123" (:session-id result))
            "should preserve session-id for CLI resume")))

    (testing "from :running-cli to :running-sdk"
      (let [state {:state :running-cli
                   :story-id 53
                   :current-task-id 75
                   :session-id "sess-123"
                   :error nil}
            result (console/transition state :running-sdk
                                       {:session-id "new-sess"})]
        (is (= :running-sdk (:state result)))
        (is (= "new-sess" (:session-id result))
            "should update session-id if provided")))

    (testing "from :error-recovery to :selecting-task"
      (let [state {:state :error-recovery
                   :story-id 53
                   :current-task-id 75
                   :session-id "sess-123"
                   :error {:type :sdk-error}}
            result (console/transition state :selecting-task)]
        (is (= :selecting-task (:state result)))
        (is (nil? (:error result))
            "should clear error on recovery")))

    (testing "from :error-recovery to :idle"
      (let [state {:state :error-recovery
                   :story-id 53
                   :current-task-id 75
                   :session-id "sess-123"
                   :error {:type :sdk-error}}
            result (console/transition state :idle)]
        (is (= :idle (:state result)))
        (is (nil? (:story-id result))
            "should clear story-id on abort")
        (is (nil? (:error result))
            "should clear error")))

    (testing "from :task-complete to :selecting-task"
      (let [state {:state :task-complete
                   :story-id 53
                   :current-task-id nil
                   :session-id "sess-123"
                   :error nil}
            result (console/transition state :selecting-task)]
        (is (= :selecting-task (:state result)))
        (is (= 53 (:story-id result))
            "should preserve story-id")))

    (testing "from :story-complete to :idle"
      (let [state {:state :story-complete
                   :story-id 53
                   :current-task-id nil
                   :session-id nil
                   :error nil}
            result (console/transition state :idle)]
        (is (= :idle (:state result)))
        (is (nil? (:story-id result))
            "should clear story-id")))

    (testing "without context argument"
      (let [state {:state :needs-input
                   :story-id 53
                   :current-task-id 75
                   :session-id "sess-123"
                   :error nil}
            result (console/transition state :running-cli)]
        (is (= :running-cli (:state result))
            "should work with 2-arity call")))))

(deftest transition-invalid-test
  (testing "transition"
    (testing "with invalid source state"
      (let [state {:state :unknown-state}
            ex (try
                 (console/transition state :idle)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex)
            "should throw exception")
        (is (= :invalid-state (:type (ex-data ex)))
            "should have :invalid-state type")
        (is (= :unknown-state (:state (ex-data ex)))
            "should include invalid state in ex-data")
        (is (= console/all-states (:valid-states (ex-data ex)))
            "should include valid states in ex-data")))

    (testing "with invalid target state"
      (let [state {:state :idle}
            ex (try
                 (console/transition state :unknown-target)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex)
            "should throw exception")
        (is (= :invalid-state (:type (ex-data ex)))
            "should have :invalid-state type")
        (is (= :unknown-target (:state (ex-data ex)))
            "should include invalid state in ex-data")))

    (testing "with invalid transition"
      (let [state {:state :idle}
            ex (try
                 (console/transition state :running-sdk)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex)
            "should throw exception")
        (is (= :invalid-transition (:type (ex-data ex)))
            "should have :invalid-transition type")
        (is (= :idle (:from-state (ex-data ex)))
            "should include from-state in ex-data")
        (is (= :running-sdk (:to-state (ex-data ex)))
            "should include to-state in ex-data")
        (is (= #{:selecting-task} (:valid-targets (ex-data ex)))
            "should include valid targets in ex-data")))

    (testing "error message is informative"
      (let [state {:state :running-sdk}
            ex (try
                 (console/transition state :idle)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (re-find #":running-sdk → :idle" (ex-message ex))
            "error message should show transition attempted")))))

;;; Mutable Wrapper Tests

(deftest transition!-test
  ;; Tests the mutable transition! wrapper function.
  ;; Verifies:
  ;; - Atom mutation on valid transitions
  ;; - History tracking with timestamps
  ;; - Context is recorded in history
  ;; - Invalid transitions throw without mutating state
  (testing "transition!"
    (testing "mutates console-state atom"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (is (= :selecting-task (console/current-state))
          "should update current state")
      (is (= 53 (:story-id @console/console-state))
          "should apply context"))

    (testing "records transition in history"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (let [history (console/state-history)]
        (is (= 1 (count history))
            "should have one history entry")
        (let [entry (first history)]
          (is (= :idle (:from entry))
              "should record source state")
          (is (= :selecting-task (:to entry))
              "should record target state")
          (is (instance? java.time.Instant (:timestamp entry))
              "should include timestamp")
          (is (= {:story-id 53} (:context entry))
              "should include context"))))

    (testing "accumulates history across transitions"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "s1" :current-task-id 75})
      (console/transition! :task-complete)
      (let [history (console/state-history)]
        (is (= 3 (count history))
            "should have three history entries")
        (is (= [:idle :selecting-task :running-sdk]
               (mapv :from history))
            "should record each source state")
        (is (= [:selecting-task :running-sdk :task-complete]
               (mapv :to history))
            "should record each target state")))

    (testing "omits context when empty"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "s1" :current-task-id 75})
      (console/transition! :task-complete)
      (let [entry (last (console/state-history))]
        (is (nil? (:context entry))
            "should not include empty context")))

    (testing "returns new state"
      (console/reset-state!)
      (let [result (console/transition! :selecting-task {:story-id 53})]
        (is (= :selecting-task (:state result))
            "should return new state map")
        (is (= 53 (:story-id result))
            "should include context in returned state")))

    (testing "throws on invalid transition without mutating"
      (console/reset-state!)
      (let [state-before @console/console-state
            ex (try
                 (console/transition! :running-sdk)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex)
            "should throw exception")
        (is (= :invalid-transition (:type (ex-data ex)))
            "should have :invalid-transition type")
        (is (= state-before @console/console-state)
            "should not mutate state on failure")))))

(deftest current-state-test
  ;; Tests the current-state query function.
  (testing "current-state"
    (testing "returns current state keyword"
      (console/reset-state!)
      (is (= :idle (console/current-state)))
      (console/transition! :selecting-task {:story-id 53})
      (is (= :selecting-task (console/current-state)))
      (console/transition! :running-sdk {:session-id "s1" :current-task-id 75})
      (is (= :running-sdk (console/current-state))))))

(deftest state-history-test
  ;; Tests the state-history query function.
  (testing "state-history"
    (testing "returns empty vector initially"
      (console/reset-state!)
      (is (= [] (console/state-history))))

    (testing "returns history vector after transitions"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "s1" :current-task-id 75})
      (let [history (console/state-history)]
        (is (= 2 (count history)))
        (is (every? #(contains? % :from) history)
            "all entries should have :from")
        (is (every? #(contains? % :to) history)
            "all entries should have :to")
        (is (every? #(contains? % :timestamp) history)
            "all entries should have :timestamp")))))

(deftest reset-state!-test
  ;; Tests the reset-state! function.
  (testing "reset-state!"
    (testing "resets to initial state"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "s1" :current-task-id 75})
      (console/reset-state!)
      (is (= :idle (console/current-state))
          "should reset state to :idle")
      (is (nil? (:story-id @console/console-state))
          "should clear story-id")
      (is (nil? (:session-id @console/console-state))
          "should clear session-id"))

    (testing "clears history"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (console/transition! :running-sdk {:session-id "s1" :current-task-id 75})
      (is (= 2 (count (console/state-history)))
          "should have history before reset")
      (console/reset-state!)
      (is (= [] (console/state-history))
          "should clear history after reset"))))

;;; Handoff Integration Tests

(defn- create-temp-handoff-file
  "Create a temp file for handoff testing."
  []
  (File/createTempFile "handoff-test" ".edn"))

(defmacro with-temp-handoff
  "Execute body with a temp handoff file, cleaning up afterward.
   Binds path-sym to the temp file path and sets *handoff-path*."
  [[path-sym temp-file-expr] & body]
  `(let [temp-file# ~temp-file-expr
         ~path-sym (.getAbsolutePath temp-file#)]
     (try
       (binding [console/*handoff-path* ~path-sym]
         ~@body)
       (finally
         (.delete temp-file#)))))

(deftest state->handoff-status-test
  ;; Tests the state->handoff-status mapping.
  ;; Verifies each console state maps to the correct handoff status.
  (testing "state->handoff-status"
    (testing "maps :running-sdk to :active"
      (is (= :active (console/state->handoff-status :running-sdk))))
    (testing "maps :needs-input to :needs-input"
      (is (= :needs-input (console/state->handoff-status :needs-input))))
    (testing "maps :running-cli to :active"
      (is (= :active (console/state->handoff-status :running-cli))))
    (testing "maps :task-complete to :completed"
      (is (= :completed (console/state->handoff-status :task-complete))))
    (testing "maps :story-complete to :completed"
      (is (= :completed (console/state->handoff-status :story-complete))))
    (testing "maps :error-recovery to :error"
      (is (= :error (console/state->handoff-status :error-recovery))))
    (testing "returns nil for :idle"
      (is (nil? (console/state->handoff-status :idle))))
    (testing "returns nil for :selecting-task"
      (is (nil? (console/state->handoff-status :selecting-task))))))

(deftest handoff-integration-test
  ;; Tests handoff file writes on state transitions.
  ;; Verifies:
  ;; - Handoff file written with correct status on relevant transitions
  ;; - Handoff file contains correct session-id, task-id, story-id
  ;; - No handoff write on :idle or :selecting-task transitions
  (testing "transition!"
    (testing "writes handoff file on :running-sdk transition"
      (with-temp-handoff [path (create-temp-handoff-file)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-1"
                                           :current-task-id 75})
        (let [state (handoff/read-handoff-state path)]
          (is (= :active (:status state)))
          (is (= "sess-1" (:session-id state)))
          (is (= 75 (:task-id state)))
          (is (= 53 (:story-id state)))
          (is (inst? (:timestamp state))))))

    (testing "writes handoff file on :needs-input transition"
      (with-temp-handoff [path (create-temp-handoff-file)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-2"
                                           :current-task-id 76})
        (console/transition! :needs-input)
        (let [state (handoff/read-handoff-state path)]
          (is (= :needs-input (:status state)))
          (is (= "sess-2" (:session-id state)))
          (is (= 76 (:task-id state))))))

    (testing "writes handoff file on :running-cli transition"
      (with-temp-handoff [path (create-temp-handoff-file)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-3"
                                           :current-task-id 77})
        (console/transition! :needs-input)
        (console/transition! :running-cli)
        (let [state (handoff/read-handoff-state path)]
          (is (= :active (:status state)))
          (is (= "sess-3" (:session-id state))))))

    (testing "writes handoff file on :task-complete transition"
      (with-temp-handoff [path (create-temp-handoff-file)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-4"
                                           :current-task-id 78})
        (console/transition! :task-complete)
        (let [state (handoff/read-handoff-state path)]
          (is (= :completed (:status state)))
          (is (= "sess-4" (:session-id state))))))

    (testing "writes handoff file on :error-recovery transition"
      (with-temp-handoff [path (create-temp-handoff-file)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-5"
                                           :current-task-id 79})
        (console/transition! :error-recovery {:error {:type :test}})
        (let [state (handoff/read-handoff-state path)]
          (is (= :error (:status state)))
          (is (= "sess-5" (:session-id state))))))

    (testing "does not write handoff file on :selecting-task transition"
      (with-temp-handoff [path (create-temp-handoff-file)]
        (handoff/clear-handoff-state path)
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (is (not (.exists (File. path)))
            "handoff file should not exist")))

    (testing "does not write handoff file on :idle transition"
      (with-temp-handoff [path (create-temp-handoff-file)]
        (handoff/clear-handoff-state path)
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-6"
                                           :current-task-id 80})
        (console/transition! :error-recovery {:error {:type :test}})
        ;; Clear the handoff file to test :idle doesn't write
        (handoff/clear-handoff-state path)
        (console/transition! :idle)
        (is (not (.exists (File. path)))
            "handoff file should not exist after :idle transition")))))