(ns task-conductor.agent-runner.events-test
  ;; Unit tests for the events namespace.
  ;;
  ;; Contracts tested:
  ;; - Event schema validates required fields correctly
  ;; - All EventType keywords are accepted
  ;; - valid-event? returns true/false without throwing
  ;; - explain-event returns nil for valid events, error map for invalid
  ;; - validate-event! throws on invalid, returns event on valid
  ;; - create-event generates valid events with automatic timestamp
  ;; - create-event accepts optional :task-id and :content
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.events :as events]))

;;; Event Schema Validation Tests

(deftest valid-event?-test
  (testing "valid-event?"
    (testing "returns true for minimal valid event"
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id "abc-123"
                   :story-id 42
                   :type :text-block}]
        (is (true? (events/valid-event? event)))))

    (testing "returns true for event with all fields"
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id "abc-123"
                   :story-id 42
                   :type :tool-use-block
                   :task-id 100
                   :content {:tool "Read" :args {:path "/x"}}}]
        (is (true? (events/valid-event? event)))))

    (testing "returns true for event with nil task-id"
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id "abc-123"
                   :story-id 42
                   :type :text-block
                   :task-id nil}]
        (is (true? (events/valid-event? event)))))

    (testing "returns false when :timestamp missing"
      (let [event {:session-id "abc" :story-id 1 :type :text-block}]
        (is (false? (events/valid-event? event)))))

    (testing "returns false when :session-id missing"
      (let [event {:timestamp (java.time.Instant/now)
                   :story-id 1
                   :type :text-block}]
        (is (false? (events/valid-event? event)))))

    (testing "returns false when :session-id empty"
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id ""
                   :story-id 1
                   :type :text-block}]
        (is (false? (events/valid-event? event)))))

    (testing "returns false when :story-id missing"
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id "abc"
                   :type :text-block}]
        (is (false? (events/valid-event? event)))))

    (testing "returns false when :type missing"
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id "abc"
                   :story-id 1}]
        (is (false? (events/valid-event? event)))))

    (testing "returns false for invalid :type"
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id "abc"
                   :story-id 1
                   :type :invalid-type}]
        (is (false? (events/valid-event? event)))))))

(deftest event-type-test
  (testing "valid-event?"
    (testing "accepts all EventType keywords"
      (doseq [event-type [:text-block :thinking-block :tool-use-block
                          :tool-result-block :result-message]]
        (let [event {:timestamp (java.time.Instant/now)
                     :session-id "abc-123"
                     :story-id 42
                     :type event-type}]
          (is (true? (events/valid-event? event))
              (str "Expected " event-type " to be valid")))))))

;;; explain-event Tests

(deftest explain-event-test
  (testing "explain-event"
    (testing "returns nil for valid event"
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id "abc-123"
                   :story-id 42
                   :type :text-block}]
        (is (nil? (events/explain-event event)))))

    (testing "returns error map with missing fields"
      (let [event {:story-id 42 :type :text-block}
            errors (events/explain-event event)]
        (is (some? errors))
        (is (contains? errors :timestamp))
        (is (contains? errors :session-id))))))

;;; validate-event! Tests

(deftest validate-event!-test
  (testing "validate-event!"
    (testing "returns event when valid"
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id "abc-123"
                   :story-id 42
                   :type :text-block}]
        (is (= event (events/validate-event! event)))))

    (testing "throws ex-info when invalid"
      (let [event {:story-id 42}]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Invalid Event"
                              (events/validate-event! event)))))

    (testing "includes :type :validation-error in ex-data"
      (let [event {:story-id 42}
            ex (try
                 (events/validate-event! event)
                 nil
                 (catch Exception e e))]
        (is (some? ex))
        (is (= :validation-error (:type (ex-data ex))))))))

;;; create-event Tests

(deftest create-event-test
  (testing "create-event"
    (testing "creates valid event with required fields"
      (let [event (events/create-event "sess-123" 42 :text-block)]
        (is (events/valid-event? event))
        (is (= "sess-123" (:session-id event)))
        (is (= 42 (:story-id event)))
        (is (= :text-block (:type event)))
        (is (inst? (:timestamp event)))))

    (testing "includes content when provided"
      (let [content {:text "Hello"}
            event (events/create-event "sess-123" 42 :text-block content)]
        (is (= content (:content event)))))

    (testing "includes task-id from opts"
      (let [event (events/create-event "sess-123" 42 :text-block nil
                                       {:task-id 100})]
        (is (= 100 (:task-id event)))))

    (testing "omits task-id when nil in opts"
      (let [event (events/create-event "sess-123" 42 :text-block nil
                                       {:task-id nil})]
        (is (not (contains? event :task-id)))))

    (testing "omits content when nil"
      (let [event (events/create-event "sess-123" 42 :text-block nil)]
        (is (not (contains? event :content)))))

    (testing "throws for invalid session-id"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid Event"
                            (events/create-event "" 42 :text-block))))

    (testing "throws for invalid event-type"
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid Event"
                            (events/create-event "sess" 42 :bad-type))))))
