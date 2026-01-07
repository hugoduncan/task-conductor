(ns task-conductor.agent-runner.console-test
  ;; Tests the console state machine transition logic.
  ;; Verifies:
  ;; - valid-transitions map defines correct transition graph
  ;; - can-transition? correctly identifies valid/invalid transitions
  ;; - transition function applies context and validates transitions
  ;; - invalid transitions throw informative errors
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.console :as console]))

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
