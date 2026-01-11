(ns task-conductor.agent-runner.flow-test
  ;; Tests for FlowDecision schema validation and FlowModel protocol.
  ;;
  ;; Contracts tested:
  ;; - FlowDecision schema validates required :action field
  ;; - FlowDecision schema validates :action enum values
  ;; - FlowDecision schema allows optional :prompt, :reason, :context
  ;; - FlowDecision schema is open for extension
  ;; - validate-decision! throws on invalid, returns unchanged on valid
  ;; - validate-continue-sdk-prompt! enforces :prompt for :continue-sdk
  ;; - validate-decision-complete! combines schema and semantic validation
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.flow :as flow]))

;;; Test Data

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
