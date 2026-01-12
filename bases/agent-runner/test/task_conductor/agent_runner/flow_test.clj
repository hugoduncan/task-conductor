(ns task-conductor.agent-runner.flow-test
  ;; Tests for FlowDecision schema validation, FlowModel protocol,
  ;; and DefaultFlowModel implementation.
  ;;
  ;; Contracts tested:
  ;; - FlowDecision schema validates required :action field
  ;; - FlowDecision schema validates :action enum values
  ;; - FlowDecision schema allows optional :prompt, :reason, :context
  ;; - FlowDecision schema is open for extension
  ;; - validate-decision! throws on invalid, returns unchanged on valid
  ;; - validate-continue-sdk-prompt! enforces :prompt for :continue-sdk
  ;; - validate-decision-complete! combines schema and semantic validation
  ;; - DefaultFlowModel.initial-prompt generates execute-story-child prompt
  ;; - DefaultFlowModel.on-cli-return generates resume prompt from CLI status
  ;; - DefaultFlowModel.on-sdk-complete returns :task-done
  ;; - DefaultFlowModel.on-task-complete queries mcp-tasks and decides next action
  ;; - DefaultFlowModel.on-task-complete handles mcp-tasks errors and malformed responses
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.flow :as flow]))

;;; Story State Test Data

(def unrefined-story
  {:id 100 :type :story :status :open :meta {}})

(def refined-story-no-children
  {:id 100 :type :story :status :open :meta {:refined true}})

(def story-with-open-children
  {:id 100 :type :story :status :open :meta {:refined true}})

(def open-child
  {:id 101 :parent-id 100 :status :open})

(def closed-child
  {:id 102 :parent-id 100 :status :closed})

(def story-all-complete
  {:id 100 :type :story :status :open :meta {:refined true}})

(def story-code-reviewed
  {:id 100 :type :story :status :open
   :meta {:refined true :code-reviewed "2025-01-01T00:00:00Z"}})

(def story-with-pr
  {:id 100 :type :story :status :open
   :meta {:refined true :code-reviewed "2025-01-01T00:00:00Z" :pr-num 42}})

(def story-ready-to-merge
  {:id 100 :type :story :status :open
   :meta {:refined true :code-reviewed "2025-01-01T00:00:00Z"
          :pr-num 42 :ready-to-merge true}})

(def story-merged
  {:id 100 :type :story :status :closed
   :meta {:refined true :code-reviewed "2025-01-01T00:00:00Z"
          :pr-num 42 :pr-merged? true}})

;;; derive-story-state Tests

(deftest derive-story-state-test
  ;; Tests derive-story-state for all state derivation rules.
  ;; Contracts tested:
  ;; - Returns :unrefined-story when meta lacks :refined
  ;; - Returns :refined when refined but no children
  ;; - Returns :execute-tasks when incomplete children exist
  ;; - Returns :code-review when all children complete, no :code-reviewed
  ;; - Returns :create-pr when code-reviewed, all complete, no :pr-num
  ;; - Returns :manual-review when :pr-num present
  ;; - Returns :merge-pr when :ready-to-merge set
  ;; - Returns :story-complete when :pr-merged? true
  ;; - Precedence is honored when multiple conditions could match
  (testing "derive-story-state"
    (testing "returns :unrefined-story"
      (testing "when meta has no :refined key"
        (is (= :unrefined-story
               (flow/derive-story-state unrefined-story []))))

      (testing "when meta is nil"
        (is (= :unrefined-story
               (flow/derive-story-state {:id 1} []))))

      (testing "when :refined is false"
        (is (= :unrefined-story
               (flow/derive-story-state {:id 1 :meta {:refined false}} [])))))

    (testing "returns :refined"
      (testing "when refined but no children"
        (is (= :refined
               (flow/derive-story-state refined-story-no-children [])))))

    (testing "returns :execute-tasks"
      (testing "when has open children"
        (is (= :execute-tasks
               (flow/derive-story-state story-with-open-children [open-child]))))

      (testing "when has mix of open and closed children"
        (is (= :execute-tasks
               (flow/derive-story-state story-with-open-children
                                        [open-child closed-child]))))

      (testing "when not all children closed"
        (is (= :execute-tasks
               (flow/derive-story-state
                story-with-open-children
                [{:id 1 :status :in-progress}])))))

    (testing "returns :code-review"
      (testing "when all children closed and no :code-reviewed"
        (is (= :code-review
               (flow/derive-story-state story-all-complete [closed-child]))))

      (testing "when multiple children all closed"
        (is (= :code-review
               (flow/derive-story-state
                story-all-complete
                [{:id 1 :status :closed}
                 {:id 2 :status :closed}
                 {:id 3 :status :closed}])))))

    (testing "returns :create-pr"
      (testing "when code-reviewed and all children complete"
        (is (= :create-pr
               (flow/derive-story-state story-code-reviewed [closed-child])))))

    (testing "returns :manual-review"
      (testing "when :pr-num present"
        (is (= :manual-review
               (flow/derive-story-state story-with-pr [closed-child])))))

    (testing "returns :merge-pr"
      (testing "when :ready-to-merge set"
        (is (= :merge-pr
               (flow/derive-story-state story-ready-to-merge [closed-child])))))

    (testing "returns :story-complete"
      (testing "when :pr-merged? true"
        (is (= :story-complete
               (flow/derive-story-state story-merged [closed-child])))))

    (testing "precedence"
      (testing ":story-complete takes precedence over :merge-pr"
        (let [story {:id 1 :meta {:pr-merged? true :ready-to-merge true}}]
          (is (= :story-complete
                 (flow/derive-story-state story [closed-child])))))

      (testing ":merge-pr takes precedence over :manual-review"
        (let [story {:id 1 :meta {:ready-to-merge true :pr-num 42}}]
          (is (= :merge-pr
                 (flow/derive-story-state story [closed-child])))))

      (testing ":manual-review takes precedence over :create-pr"
        (let [story {:id 1 :meta {:pr-num 42 :code-reviewed "ts"}}]
          (is (= :manual-review
                 (flow/derive-story-state story [closed-child])))))

      (testing ":create-pr takes precedence over :code-review"
        (let [story {:id 1 :meta {:code-reviewed "ts" :refined true}}]
          (is (= :create-pr
                 (flow/derive-story-state story [closed-child])))))

      (testing ":code-review takes precedence over :execute-tasks"
        ;; If all complete, not in execute state
        (let [story {:id 1 :meta {:refined true}}]
          (is (= :code-review
                 (flow/derive-story-state story [closed-child])))))

      (testing ":execute-tasks takes precedence over :refined"
        (let [story {:id 1 :meta {:refined true}}]
          (is (= :execute-tasks
                 (flow/derive-story-state story [open-child])))))

      (testing ":refined takes precedence over :unrefined-story"
        (let [story {:id 1 :meta {:refined true}}]
          (is (= :refined
                 (flow/derive-story-state story []))))))))

;;; FlowDecision Test Data

(def valid-decision
  {:action :task-done})

(def valid-decision-with-prompt
  {:action :continue-sdk
   :prompt "Continue the task"})

(def valid-decision-full
  {:action :continue-sdk
   :prompt "Resume after user input"
   :reason "CLI returned with completion"
   :context {:next-state :running-sdk}})

;;; FlowDecision Schema Tests

(deftest valid-decision?-test
  (testing "valid-decision?"
    (testing "accepts minimal valid decision"
      (is (flow/valid-decision? valid-decision)))

    (testing "accepts decision with all optional fields"
      (is (flow/valid-decision? valid-decision-full)))

    (testing "accepts all valid action values"
      (doseq [action [:continue-sdk :hand-to-cli :task-done
                      :story-done :error :pause]]
        (is (flow/valid-decision? {:action action})
            (str "should accept action " action))))

    (testing "allows extension fields"
      (is (flow/valid-decision?
           (assoc valid-decision :custom-field "value"))))

    (testing "rejects missing :action"
      (is (not (flow/valid-decision? {}))))

    (testing "rejects invalid :action value"
      (is (not (flow/valid-decision? {:action :invalid}))))

    (testing "rejects wrong type for :action"
      (is (not (flow/valid-decision? {:action "task-done"}))))

    (testing "rejects wrong type for :prompt"
      (is (not (flow/valid-decision?
                {:action :continue-sdk :prompt 123}))))

    (testing "rejects wrong type for :reason"
      (is (not (flow/valid-decision?
                {:action :task-done :reason 123}))))

    (testing "rejects wrong type for :context"
      (is (not (flow/valid-decision?
                {:action :task-done :context "not-a-map"}))))))

(deftest explain-decision-test
  (testing "explain-decision"
    (testing "returns nil for valid decision"
      (is (nil? (flow/explain-decision valid-decision))))

    (testing "returns errors for missing :action"
      (let [explanation (flow/explain-decision {})]
        (is (some? explanation))
        (is (contains? explanation :action))))

    (testing "returns errors for invalid :action value"
      (let [explanation (flow/explain-decision {:action :invalid})]
        (is (some? explanation))
        (is (contains? explanation :action))))))

(deftest validate-decision!-test
  (testing "validate-decision!"
    (testing "returns valid decision unchanged"
      (is (= valid-decision (flow/validate-decision! valid-decision)))
      (is (= valid-decision-full (flow/validate-decision! valid-decision-full))))

    (testing "throws on missing :action"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid FlowDecision"
                            (flow/validate-decision! {}))))

    (testing "throws on invalid :action with error details"
      (try
        (flow/validate-decision! {:action :invalid})
        (is false "should have thrown")
        (catch clojure.lang.ExceptionInfo e
          (is (= :validation-error (:type (ex-data e))))
          (is (some? (:errors (ex-data e))))
          (is (= {:action :invalid} (:decision (ex-data e)))))))))

;;; Semantic Validation Tests

(deftest validate-continue-sdk-prompt!-test
  (testing "validate-continue-sdk-prompt!"
    (testing "returns decision with valid prompt unchanged"
      (is (= valid-decision-with-prompt
             (flow/validate-continue-sdk-prompt! valid-decision-with-prompt))))

    (testing "returns non-continue-sdk decisions unchanged"
      (doseq [action [:hand-to-cli :task-done :story-done :error :pause]]
        (let [decision {:action action}]
          (is (= decision (flow/validate-continue-sdk-prompt! decision))
              (str "should pass " action " without prompt")))))

    (testing "throws on :continue-sdk without :prompt"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"requires non-empty :prompt"
                            (flow/validate-continue-sdk-prompt!
                             {:action :continue-sdk}))))

    (testing "throws on :continue-sdk with empty :prompt"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"requires non-empty :prompt"
                            (flow/validate-continue-sdk-prompt!
                             {:action :continue-sdk :prompt ""}))))

    (testing "throws on :continue-sdk with nil :prompt"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"requires non-empty :prompt"
                            (flow/validate-continue-sdk-prompt!
                             {:action :continue-sdk :prompt nil}))))))

(deftest validate-decision-complete!-test
  (testing "validate-decision-complete!"
    (testing "returns valid decision unchanged"
      (is (= valid-decision-full
             (flow/validate-decision-complete! valid-decision-full))))

    (testing "validates schema first"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid FlowDecision"
                            (flow/validate-decision-complete! {}))))

    (testing "validates prompt requirement for :continue-sdk"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"requires non-empty :prompt"
                            (flow/validate-decision-complete!
                             {:action :continue-sdk}))))))

;;; Action Enum Tests

(deftest action-enum-test
  (testing "Action enum"
    (testing "includes all expected values"
      (let [expected #{:continue-sdk :hand-to-cli :task-done
                       :story-done :error :pause}]
        (doseq [action expected]
          (is (flow/valid-decision? {:action action})
              (str action " should be valid")))))))

;;; Prompt Generation Helper Tests

(deftest build-resume-prompt-test
  (testing "build-resume-prompt"
    (testing "with status only"
      (let [result (flow/build-resume-prompt {:status :completed})]
        (is (string? result))
        (is (re-find #"CLI returned with status completed" result))
        (is (re-find #"Continue the task\." result))))

    (testing "with status and reason"
      (let [result (flow/build-resume-prompt
                    {:status :completed :reason "User approved"})]
        (is (re-find #"CLI returned with status completed: User approved" result))
        (is (re-find #"Continue the task\." result))))

    (testing "with status and question (uses question as reason)"
      (let [result (flow/build-resume-prompt
                    {:status :needs-input :question "What next?"})]
        (is (re-find #"CLI returned with status needs-input: What next\?" result))))

    (testing "with shared-context (single item)"
      (let [result (flow/build-resume-prompt
                    {:status :completed}
                    ["Previous task added feature X"])]
        (is (re-find #"Context from previous tasks:" result))
        (is (re-find #"Previous task added feature X" result))))

    (testing "with shared-context (multiple items)"
      (let [result (flow/build-resume-prompt
                    {:status :completed}
                    ["Item 1" "Item 2" "Item 3"])]
        (is (re-find #"Context from previous tasks:" result))
        (is (re-find #"Item 1" result))
        (is (re-find #"Item 2" result))
        (is (re-find #"Item 3" result))))

    (testing "with empty shared-context (no context section)"
      (let [result (flow/build-resume-prompt {:status :completed} [])]
        (is (not (re-find #"Context from previous tasks:" result)))))

    (testing "with nil shared-context (no context section)"
      (let [result (flow/build-resume-prompt {:status :completed} nil)]
        (is (not (re-find #"Context from previous tasks:" result)))))

    (testing "single-arity version (no shared-context)"
      (let [result (flow/build-resume-prompt {:status :completed})]
        (is (re-find #"CLI returned with status completed" result))
        (is (not (re-find #"Context from previous tasks:" result)))))))

;;; DefaultFlowModel Tests

(defn mock-run-mcp-tasks
  "Create a mock run-mcp-tasks-fn that returns the given response."
  [response]
  (fn [& _args] response))

(defn mock-run-mcp-tasks-with-capture
  "Create a mock that captures args and returns response."
  [response captured-atom]
  (fn [& args]
    (reset! captured-atom (vec args))
    response))

(deftest default-flow-model-test
  (testing "default-flow-model"
    (testing "creates a DefaultFlowModel instance"
      (let [fm (flow/default-flow-model (mock-run-mcp-tasks {}))]
        (is (some? fm))
        (is (satisfies? flow/FlowModel fm))))))

(deftest initial-prompt-test
  (testing "initial-prompt"
    (testing "generates execute-story-child prompt with parent-id"
      (let [fm (flow/default-flow-model (mock-run-mcp-tasks {}))
            task-info {:id 120 :parent-id 91}
            console-state {:story-id 91}
            result (flow/initial-prompt fm task-info console-state)]
        (is (= :continue-sdk (:action result)))
        (is (= "/mcp-tasks:execute-story-child 91" (:prompt result)))))

    (testing "returns valid FlowDecision"
      (let [fm (flow/default-flow-model (mock-run-mcp-tasks {}))
            result (flow/initial-prompt fm {:parent-id 57} {})]
        (is (flow/valid-decision? result))
        (is (nil? (flow/explain-decision result)))))))

(deftest on-cli-return-test
  (testing "on-cli-return"
    (let [fm (flow/default-flow-model (mock-run-mcp-tasks {}))]

      (testing "generates resume prompt with status"
        (let [cli-status {:status :completed :timestamp "2025-01-01T00:00:00Z"}
              result (flow/on-cli-return fm cli-status {})]
          (is (= :continue-sdk (:action result)))
          (is (re-find #"completed" (:prompt result)))
          (is (re-find #"Continue the task" (:prompt result)))))

      (testing "includes reason in prompt when present"
        (let [cli-status {:status :completed :reason "User approved"}
              result (flow/on-cli-return fm cli-status {})]
          (is (= :continue-sdk (:action result)))
          (is (re-find #"User approved" (:prompt result)))))

      (testing "includes question in prompt when present"
        (let [cli-status {:status :needs-input :question "What next?"}
              result (flow/on-cli-return fm cli-status {})]
          (is (= :continue-sdk (:action result)))
          (is (re-find #"What next\?" (:prompt result)))))

      (testing "includes shared-context from console-state"
        (let [cli-status {:status :completed}
              console-state {:shared-context ["Context item 1" "Context item 2"]}
              result (flow/on-cli-return fm cli-status console-state)]
          (is (= :continue-sdk (:action result)))
          (is (re-find #"Context from previous tasks:" (:prompt result)))
          (is (re-find #"Context item 1" (:prompt result)))
          (is (re-find #"Context item 2" (:prompt result)))))

      (testing "returns valid FlowDecision"
        (let [result (flow/on-cli-return fm {:status :completed} {})]
          (is (flow/valid-decision? result)))))))

(deftest on-sdk-complete-test
  (testing "on-sdk-complete"
    (let [fm (flow/default-flow-model (mock-run-mcp-tasks {}))]

      (testing "returns :task-done action"
        (let [sdk-result {:messages [] :result {}}
              result (flow/on-sdk-complete fm sdk-result {})]
          (is (= :task-done (:action result)))))

      (testing "returns valid FlowDecision"
        (let [result (flow/on-sdk-complete fm {} {})]
          (is (flow/valid-decision? result)))))))

(deftest on-task-complete-test
  (testing "on-task-complete"
    (testing "when next task is available"
      (let [captured (atom nil)
            mock-fn (mock-run-mcp-tasks-with-capture
                     {:tasks [{:id 121 :title "Next task"}]}
                     captured)
            fm (flow/default-flow-model mock-fn)
            console-state {:story-id 91}
            result (flow/on-task-complete fm console-state)]

        (testing "queries mcp-tasks with correct arguments"
          (is (= ["list" "--parent-id" "91" "--blocked" "false" "--limit" "1"]
                 @captured)))

        (testing "returns :continue-sdk with prompt"
          (is (= :continue-sdk (:action result)))
          (is (= "/mcp-tasks:execute-story-child 91" (:prompt result))))))

    (testing "when no more tasks available"
      (let [fm (flow/default-flow-model
                (mock-run-mcp-tasks {:tasks []}))
            result (flow/on-task-complete fm {:story-id 91})]

        (testing "returns :story-done"
          (is (= :story-done (:action result))))

        (testing "includes reason"
          (is (some? (:reason result))))))

    (testing "returns valid FlowDecision in both cases"
      (let [fm-with-tasks (flow/default-flow-model
                           (mock-run-mcp-tasks {:tasks [{:id 1}]}))
            fm-no-tasks (flow/default-flow-model
                         (mock-run-mcp-tasks {:tasks []}))]
        (is (flow/valid-decision?
             (flow/on-task-complete fm-with-tasks {:story-id 1})))
        (is (flow/valid-decision?
             (flow/on-task-complete fm-no-tasks {:story-id 1})))))

    (testing "when mcp-tasks returns error"
      (let [fm (flow/default-flow-model
                (mock-run-mcp-tasks {:error "Connection failed"}))
            result (flow/on-task-complete fm {:story-id 91})]

        (testing "returns :error action"
          (is (= :error (:action result))))

        (testing "includes error message in reason"
          (is (= "Connection failed" (:reason result))))

        (testing "returns valid FlowDecision"
          (is (flow/valid-decision? result)))))

    (testing "when mcp-tasks returns unexpected format"
      (let [fm (flow/default-flow-model
                (mock-run-mcp-tasks {:unexpected "format"}))
            result (flow/on-task-complete fm {:story-id 91})]

        (testing "returns :error action"
          (is (= :error (:action result))))

        (testing "includes format info in reason"
          (is (re-find #"Unexpected mcp-tasks response format"
                       (:reason result))))

        (testing "returns valid FlowDecision"
          (is (flow/valid-decision? result)))))

    (testing "when run-mcp-tasks-fn throws exception"
      (let [fm (flow/default-flow-model
                (fn [& _] (throw (ex-info "Network error" {}))))
            result (try
                     (flow/on-task-complete fm {:story-id 91})
                     (catch Exception e
                       {:caught-exception e}))]
        (testing "exception propagates to caller"
          (is (some? (:caught-exception result)))
          (is (= "Network error"
                 (.getMessage (:caught-exception result)))))))

    (testing "when run-mcp-tasks-fn returns nil"
      (let [fm (flow/default-flow-model (mock-run-mcp-tasks nil))
            result (flow/on-task-complete fm {:story-id 91})]

        (testing "returns :error action"
          (is (= :error (:action result))))

        (testing "includes format info in reason"
          (is (re-find #"Unexpected mcp-tasks response format"
                       (:reason result))))

        (testing "returns valid FlowDecision"
          (is (flow/valid-decision? result)))))

    (testing "when :tasks is malformed (non-collection)"
      (let [fm (flow/default-flow-model
                (mock-run-mcp-tasks {:tasks "not-a-list"}))
            result (flow/on-task-complete fm {:story-id 91})]
        ;; (first "not-a-list") returns \n - current behavior returns :continue-sdk
        ;; This tests current behavior; consider adding validation if undesired
        (testing "treats first char as truthy task"
          (is (= :continue-sdk (:action result))))))))
