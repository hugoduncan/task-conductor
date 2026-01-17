(ns task-conductor.agent-runner.console-test
  ;; Tests the console state machine transition logic.
  ;; Verifies:
  ;; - valid-transitions map defines correct transition graph
  ;; - can-transition? correctly identifies valid/invalid transitions
  ;; - transition function applies context and validates transitions
  ;; - invalid transitions throw informative errors
  ;; - handoff file integration writes state on relevant transitions
  ;; - launch-cli-resume constructs correct command and returns exit code
  ;; - hand-to-cli orchestrates state transitions and handles CLI exit
  ;; - hand-to-cli async mode with dev-env returns immediately
  (:require
   [babashka.process :as p]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.console :as console]
   [task-conductor.agent-runner.handoff :as handoff]
   [task-conductor.dev-env.interface :as dev-env])
  (:import
   [java.io File]
   [java.time Instant]))

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
      (is (= #{:running-sdk :story-complete :error-recovery}
             (get console/valid-transitions :selecting-task))
          "should allow :running-sdk, :story-complete, or :error-recovery"))

    (testing "from :running-sdk"
      (is (= #{:needs-input :task-complete :error-recovery}
             (get console/valid-transitions :running-sdk))
          "should allow :needs-input, :task-complete, or :error-recovery"))

    (testing "from :needs-input"
      (is (= #{:running-cli :error-recovery}
             (get console/valid-transitions :needs-input))
          "should allow :running-cli or :error-recovery"))

    (testing "from :running-cli"
      (is (= #{:running-sdk :error-recovery}
             (get console/valid-transitions :running-cli))
          "should allow :running-sdk or :error-recovery"))

    (testing "from :error-recovery"
      (is (= #{:selecting-task :running-sdk :idle}
             (get console/valid-transitions :error-recovery))
          "should allow :selecting-task, :running-sdk, or :idle"))

    (testing "from :task-complete"
      (is (= #{:selecting-task :story-complete :error-recovery}
             (get console/valid-transitions :task-complete))
          "should allow :selecting-task, :story-complete, or :error-recovery"))

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

;;; CLI Handoff Tests

(deftest launch-cli-resume-test
  ;; Tests launch-cli-resume constructs correct command and returns exit code.
  ;; Uses with-redefs to mock babashka.process/process.
  (testing "launch-cli-resume"
    (testing "constructs correct command with session-id"
      (let [captured-cmd (atom nil)]
        (with-redefs [p/process (fn [cmd _opts]
                                  (reset! captured-cmd cmd)
                                  (delay {:exit 0}))]
          (console/launch-cli-resume "test-session-123")
          (is (= ["claude" "--resume" "test-session-123"]
                 @captured-cmd)))))

    (testing "returns exit code 0 on success"
      (with-redefs [p/process (fn [_cmd _opts]
                                (delay {:exit 0}))]
        (is (= 0 (console/launch-cli-resume "session-abc")))))

    (testing "returns non-zero exit code on failure"
      (with-redefs [p/process (fn [_cmd _opts]
                                (delay {:exit 1}))]
        (is (= 1 (console/launch-cli-resume "session-def")))))

    (testing "passes :inherit true for stdio inheritance"
      (let [captured-opts (atom nil)]
        (with-redefs [p/process (fn [_cmd opts]
                                  (reset! captured-opts opts)
                                  (delay {:exit 0}))]
          (console/launch-cli-resume "session-xyz")
          (is (= {:inherit true} @captured-opts)))))))

(def mock-hook-status-completed
  {:status :completed
   :timestamp (Instant/parse "2024-01-15T10:30:00Z")})

(def mock-hook-status-error
  {:status :error
   :timestamp (Instant/parse "2024-01-15T10:30:00Z")
   :reason :tool-error})

(deftest hand-to-cli-test
  ;; Tests hand-to-cli orchestrates state transitions and handles CLI exit.
  ;; Verifies:
  ;; - Transitions through :needs-input to :running-cli
  ;; - Returns {:state <map> :cli-status <hook-status>}
  ;; - Error recovery based on exit code, hook status, or missing hook
  ;; - Throws when not in :running-sdk state
  (testing "hand-to-cli"
    (testing "transitions through :needs-input to :running-cli"
      (with-redefs [p/process (fn [_cmd _opts] (delay {:exit 0}))
                    handoff/read-hook-status (constantly mock-hook-status-completed)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-1"
                                           :current-task-id 75})
        (console/hand-to-cli)
        (let [history (console/state-history)]
          (is (some #(= :needs-input (:to %)) history)
              "should transition through :needs-input")
          (is (some #(= :running-cli (:to %)) history)
              "should transition through :running-cli"))))

    (testing "returns extended result with :state and :cli-status"
      (with-redefs [p/process (fn [_cmd _opts] (delay {:exit 0}))
                    handoff/read-hook-status (constantly mock-hook-status-completed)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-2"
                                           :current-task-id 76})
        (let [result (console/hand-to-cli)]
          (is (map? result)
              "should return a map")
          (is (contains? result :state)
              "should contain :state key")
          (is (contains? result :cli-status)
              "should contain :cli-status key")
          (is (map? (:state result))
              ":state should be a map")
          (is (= mock-hook-status-completed (:cli-status result))
              ":cli-status should be the hook status"))))

    (testing "returns to :running-sdk on exit code 0 with :completed hook status"
      (with-redefs [p/process (fn [_cmd _opts] (delay {:exit 0}))
                    handoff/read-hook-status (constantly mock-hook-status-completed)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-3"
                                           :current-task-id 77})
        (let [result (console/hand-to-cli)]
          (is (= :running-sdk (get-in result [:state :state]))
              "should return to :running-sdk")
          (is (= :running-sdk (console/current-state))
              "current state should be :running-sdk"))))

    (testing "transitions to :error-recovery on non-zero exit code"
      (with-redefs [p/process (fn [_cmd _opts] (delay {:exit 1}))
                    handoff/read-hook-status (constantly mock-hook-status-completed)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-4"
                                           :current-task-id 78})
        (let [result (console/hand-to-cli)]
          (is (= :error-recovery (get-in result [:state :state]))
              "should transition to :error-recovery")
          (is (= :cli-error (get-in result [:state :error :type]))
              "error type should be :cli-error")
          (is (= 1 (get-in result [:state :error :exit-code]))
              "should include exit code"))))

    (testing "transitions to :error-recovery on hook status :error"
      (with-redefs [p/process (fn [_cmd _opts] (delay {:exit 0}))
                    handoff/read-hook-status (constantly mock-hook-status-error)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-5"
                                           :current-task-id 79})
        (let [result (console/hand-to-cli)]
          (is (= :error-recovery (get-in result [:state :state]))
              "should transition to :error-recovery")
          (is (= :hook-error (get-in result [:state :error :type]))
              "error type should be :hook-error")
          (is (= mock-hook-status-error (get-in result [:state :error :hook-status]))
              "should include hook status in error"))))

    (testing "transitions to :error-recovery with :cli-killed when hook status is nil"
      (with-redefs [p/process (fn [_cmd _opts] (delay {:exit 0}))
                    handoff/read-hook-status (constantly nil)]
        (console/reset-state!)
        (console/transition! :selecting-task {:story-id 53})
        (console/transition! :running-sdk {:session-id "sess-6"
                                           :current-task-id 80})
        (let [result (console/hand-to-cli)]
          (is (= :error-recovery (get-in result [:state :state]))
              "should transition to :error-recovery")
          (is (= :cli-killed (get-in result [:state :error :type]))
              "error type should be :cli-killed")
          (is (= :cli-killed (get-in result [:state :error :reason]))
              "should include :cli-killed reason")
          (is (nil? (:cli-status result))
              ":cli-status should be nil"))))

    (testing "uses session-id from console-state"
      (let [captured-session (atom nil)]
        (with-redefs [p/process (fn [cmd _opts]
                                  (reset! captured-session (nth cmd 2))
                                  (delay {:exit 0}))
                      handoff/read-hook-status (constantly mock-hook-status-completed)]
          (console/reset-state!)
          (console/transition! :selecting-task {:story-id 53})
          (console/transition! :running-sdk {:session-id "my-session-id"
                                             :current-task-id 81})
          (console/hand-to-cli)
          (is (= "my-session-id" @captured-session)
              "should use session-id from state"))))

    (testing "throws when not in :running-sdk state"
      (console/reset-state!)
      (let [ex (try
                 (console/hand-to-cli)
                 nil
                 (catch clojure.lang.ExceptionInfo e e))]
        (is (some? ex)
            "should throw exception")
        (is (= :invalid-state (:type (ex-data ex)))
            "should have :invalid-state type")
        (is (= :idle (:current-state (ex-data ex)))
            "should include current state")
        (is (= :running-sdk (:required-state (ex-data ex)))
            "should include required state")))))

;;; Mock DevEnv for async tests

(defrecord MockDevEnv [calls]
  dev-env/DevEnv
  (open-cli-session [_this opts callback]
    (swap! calls conj {:method :open-cli-session :opts opts :callback callback})
    {:status :requested})
  (close-session [_this session-id]
    (swap! calls conj {:method :close-session :session-id session-id})
    {:status :requested}))

(defn create-mock-dev-env
  "Create a mock DevEnv that records calls."
  []
  (->MockDevEnv (atom [])))

(deftest hand-to-cli-async-test
  ;; Tests hand-to-cli async mode with dev-env.
  ;; In async mode:
  ;; - Returns {:status :running} immediately when dev-env provided
  ;; - Passes correct opts to dev-env/open-cli-session with nil callback
  ;; - Starts watching handoff file for status changes
  ;; - File watcher invokes idle-callback on :idle status
  ;; - File watcher transitions to :running-sdk on :completed status
  ;; - File watcher transitions to :error-recovery on :error status
  (testing "hand-to-cli"
    (testing "with dev-env"
      (testing "returns {:status :running} immediately"
        (let [mock-env (create-mock-dev-env)]
          (console/reset-state!)
          (console/transition! :selecting-task {:story-id 53})
          (console/transition! :running-sdk {:session-id "sess-async-1"
                                             :current-task-id 90})
          (let [result (console/hand-to-cli {:dev-env mock-env})]
            (is (= {:status :running} result)
                "should return :running status")
            (is (= :running-cli (console/current-state))
                "state should be :running-cli"))))

      (testing "passes correct opts to open-cli-session with nil callback"
        (let [mock-env (create-mock-dev-env)]
          (console/reset-state!)
          (console/transition! :selecting-task {:story-id 53})
          (console/transition! :running-sdk {:session-id "sess-async-2"
                                             :current-task-id 91})
          (console/hand-to-cli {:dev-env mock-env
                                :prompt "Test prompt"})
          (let [call (first @(:calls mock-env))
                opts (:opts call)]
            (is (= :open-cli-session (:method call))
                "should call open-cli-session")
            (is (= "sess-async-2" (:session-id opts))
                "should pass session-id")
            (is (= "Test prompt" (:prompt opts))
                "should pass prompt")
            (is (string? (:working-dir opts))
                "should include working-dir")
            (is (nil? (:callback call))
                "callback should be nil (fire-and-forget)"))))

      ;; Note: Testing file watcher behavior would require mocking handoff/watch-hook-status-file
      ;; The actual state transitions happen when the watcher invokes its callback
      ;; For now, we test that the dev-env is called correctly and state starts at :running-cli

      (testing "works without user callbacks"
        (let [mock-env (create-mock-dev-env)]
          (console/reset-state!)
          (console/transition! :selecting-task {:story-id 53})
          (console/transition! :running-sdk {:session-id "sess-async-7"
                                             :current-task-id 96})
          (console/hand-to-cli {:dev-env mock-env})
          (is (= :running-cli (console/current-state))
              "state should be :running-cli")
          ;; Verify dev-env was called
          (is (= 1 (count @(:calls mock-env)))
              "dev-env should be called once"))))))

;;; Pause Control Tests

(deftest paused?-test
  ;; Tests the paused? predicate.
  ;; Verifies it returns the current pause state from the console-state atom.
  (testing "paused?"
    (testing "returns false initially"
      (console/reset-state!)
      (is (false? (console/paused?))))

    (testing "returns true after set-paused!"
      (console/reset-state!)
      (console/set-paused!)
      (is (true? (console/paused?))))

    (testing "returns false after clear-paused!"
      (console/reset-state!)
      (console/set-paused!)
      (console/clear-paused!)
      (is (false? (console/paused?))))))

(deftest set-paused!-test
  ;; Tests the set-paused! function.
  ;; Verifies it sets :paused to true and returns true.
  (testing "set-paused!"
    (testing "sets :paused to true"
      (console/reset-state!)
      (console/set-paused!)
      (is (true? (:paused @console/console-state))))

    (testing "returns true"
      (console/reset-state!)
      (is (true? (console/set-paused!))))

    (testing "is idempotent"
      (console/reset-state!)
      (console/set-paused!)
      (console/set-paused!)
      (is (true? (console/paused?))))))

(deftest clear-paused!-test
  ;; Tests the clear-paused! function.
  ;; Verifies it sets :paused to false and returns false.
  (testing "clear-paused!"
    (testing "sets :paused to false"
      (console/reset-state!)
      (console/set-paused!)
      (console/clear-paused!)
      (is (false? (:paused @console/console-state))))

    (testing "returns false"
      (console/reset-state!)
      (console/set-paused!)
      (is (false? (console/clear-paused!))))

    (testing "is idempotent"
      (console/reset-state!)
      (console/clear-paused!)
      (console/clear-paused!)
      (is (false? (console/paused?))))))

;;; Session Tracking Tests

(deftest record-session!-test
  ;; Tests the record-session! function.
  ;; Verifies session entries are appended to :sessions with correct structure.
  (testing "record-session!"
    (testing "appends session entry to :sessions"
      (console/reset-state!)
      (console/record-session! "sess-1" 42)
      (is (= 1 (count (:sessions @console/console-state)))))

    (testing "accumulates multiple sessions"
      (console/reset-state!)
      (console/record-session! "sess-1" 42)
      (console/record-session! "sess-2" 43)
      (console/record-session! "sess-3" 44)
      (is (= 3 (count (:sessions @console/console-state)))))

    (testing "creates entry with correct structure"
      (console/reset-state!)
      (console/transition! :selecting-task {:story-id 53})
      (let [ts (Instant/parse "2024-01-15T10:30:00Z")]
        (console/record-session! "sess-abc" 99 ts)
        (let [entry (first (:sessions @console/console-state))]
          (is (= "sess-abc" (:session-id entry)))
          (is (= 99 (:task-id entry)))
          (is (= 53 (:story-id entry)))
          (is (= ts (:timestamp entry))))))

    (testing "includes nil story-id when not in a story"
      (console/reset-state!)
      (console/record-session! "sess-xyz" 100)
      (let [entry (first (:sessions @console/console-state))]
        (is (nil? (:story-id entry)))))

    (testing "generates timestamp when not provided"
      (console/reset-state!)
      (console/record-session! "sess-xyz" 100)
      (let [entry (first (:sessions @console/console-state))]
        (is (instance? Instant (:timestamp entry)))))

    (testing "returns updated sessions vector"
      (console/reset-state!)
      (let [result (console/record-session! "sess-1" 42)]
        (is (vector? result))
        (is (= 1 (count result)))
        (is (= "sess-1" (:session-id (first result))))))))

(deftest reset-state!-clears-pause-and-sessions-test
  ;; Tests that reset-state! clears the new :paused and :sessions fields.
  (testing "reset-state!"
    (testing "clears :paused flag"
      (console/reset-state!)
      (console/set-paused!)
      (is (true? (console/paused?)))
      (console/reset-state!)
      (is (false? (console/paused?))))

    (testing "clears :sessions vector"
      (console/reset-state!)
      (console/record-session! "sess-1" 42)
      (console/record-session! "sess-2" 43)
      (is (= 2 (count (:sessions @console/console-state))))
      (console/reset-state!)
      (is (= [] (:sessions @console/console-state))))))