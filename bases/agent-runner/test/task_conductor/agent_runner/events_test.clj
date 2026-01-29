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
  ;; - flush-events! writes session events to file
  ;; - load-session-events reads events from file with round-trip fidelity
  ;; - make-event-callback creates callback that captures SDK messages
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.events :as events]
   [task-conductor.claude-agent-sdk.interface :as sdk])
  (:import
   [java.io File]
   [java.nio.file Files]
   [java.nio.file.attribute FileAttribute]))

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

;;; Ring Buffer Storage Tests

(deftest add-event!-test
  ;; Tests add-event! validation and buffer addition behavior.
  ;; Contracts: validates events before adding, returns added event.
  (testing "add-event!"
    (testing "adds valid event to buffer"
      (events/clear-events!)
      (let [event {:timestamp (java.time.Instant/now)
                   :session-id "sess-1"
                   :story-id 42
                   :type :text-block}
            result (events/add-event! event)]
        (is (= event result))
        (is (= [event] (events/get-events)))))

    (testing "throws for invalid event"
      (events/clear-events!)
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Invalid Event"
                            (events/add-event! {:invalid "event"}))))

    (testing "accumulates multiple events"
      (events/clear-events!)
      (let [e1 (events/create-event "sess-1" 42 :text-block)
            e2 (events/create-event "sess-1" 42 :tool-use-block)]
        (events/add-event! e1)
        (events/add-event! e2)
        (is (= [e1 e2] (events/get-events)))))))

(deftest ring-buffer-overflow-test
  ;; Tests ring buffer drops oldest events when capacity exceeded.
  ;; Contract: buffer size never exceeds *max-buffer-size*.
  (testing "ring buffer"
    (testing "drops oldest events when full"
      (events/clear-events!)
      (binding [events/*max-buffer-size* 3]
        (let [e1 (events/create-event "s1" 1 :text-block "first")
              e2 (events/create-event "s1" 1 :text-block "second")
              e3 (events/create-event "s1" 1 :text-block "third")
              e4 (events/create-event "s1" 1 :text-block "fourth")]
          (events/add-event! e1)
          (events/add-event! e2)
          (events/add-event! e3)
          (is (= 3 (count (events/get-events))))
          (events/add-event! e4)
          (is (= 3 (count (events/get-events))))
          (is (= [e2 e3 e4] (events/get-events))))))))

(deftest get-events-test
  ;; Tests event retrieval and filtering.
  ;; Contracts: returns all events when no filter, filters by type/session/story.
  (testing "get-events"
    (testing "returns all events without filter"
      (events/clear-events!)
      (let [e1 (events/create-event "sess-1" 42 :text-block)
            e2 (events/create-event "sess-2" 43 :tool-use-block)]
        (events/add-event! e1)
        (events/add-event! e2)
        (is (= [e1 e2] (events/get-events)))
        (is (= [e1 e2] (events/get-events {})))))

    (testing "filters by :type"
      (events/clear-events!)
      (let [e1 (events/create-event "sess-1" 42 :text-block)
            e2 (events/create-event "sess-1" 42 :tool-use-block)
            e3 (events/create-event "sess-1" 42 :text-block)]
        (events/add-event! e1)
        (events/add-event! e2)
        (events/add-event! e3)
        (is (= [e1 e3] (events/get-events {:type :text-block})))
        (is (= [e2] (events/get-events {:type :tool-use-block})))))

    (testing "filters by :session-id"
      (events/clear-events!)
      (let [e1 (events/create-event "sess-A" 42 :text-block)
            e2 (events/create-event "sess-B" 42 :text-block)
            e3 (events/create-event "sess-A" 42 :text-block)]
        (events/add-event! e1)
        (events/add-event! e2)
        (events/add-event! e3)
        (is (= [e1 e3] (events/get-events {:session-id "sess-A"})))))

    (testing "filters by :story-id"
      (events/clear-events!)
      (let [e1 (events/create-event "sess-1" 100 :text-block)
            e2 (events/create-event "sess-1" 200 :text-block)
            e3 (events/create-event "sess-1" 100 :text-block)]
        (events/add-event! e1)
        (events/add-event! e2)
        (events/add-event! e3)
        (is (= [e1 e3] (events/get-events {:story-id 100})))))

    (testing "combines multiple filters"
      (events/clear-events!)
      (let [e1 (events/create-event "sess-A" 100 :text-block)
            e2 (events/create-event "sess-A" 100 :tool-use-block)
            e3 (events/create-event "sess-B" 100 :text-block)
            e4 (events/create-event "sess-A" 200 :text-block)]
        (events/add-event! e1)
        (events/add-event! e2)
        (events/add-event! e3)
        (events/add-event! e4)
        (is (= [e1] (events/get-events {:session-id "sess-A"
                                        :story-id 100
                                        :type :text-block})))))))

(deftest clear-events!-test
  ;; Tests buffer clearing.
  ;; Contract: empties buffer, returns nil.
  (testing "clear-events!"
    (testing "empties the buffer"
      (events/add-event! (events/create-event "sess" 1 :text-block))
      (is (seq (events/get-events)))
      (is (nil? (events/clear-events!)))
      (is (empty? (events/get-events))))))

;;; Timestamp Serialization Tests

(deftest timestamp-serialization-test
  ;; Tests timestamp conversion round-trip.
  ;; Contract: instant->iso8601 and iso8601->instant are inverse operations.
  (testing "timestamp serialization"
    (testing "round-trips Instant through ISO-8601"
      (let [original (java.time.Instant/now)
            serialized (events/instant->iso8601 original)
            deserialized (events/iso8601->instant serialized)]
        (is (string? serialized))
        (is (inst? deserialized))
        (is (= original deserialized))))

    (testing "produces valid ISO-8601 format"
      (let [inst (java.time.Instant/parse "2024-01-15T10:30:00Z")
            result (events/instant->iso8601 inst)]
        (is (= "2024-01-15T10:30:00Z" result))))))

;;; File I/O Tests

(defn- create-temp-dir
  "Create a temporary directory for testing. Returns the path as a string."
  []
  (str (Files/createTempDirectory "events-test"
                                  (into-array FileAttribute []))))

(defn- delete-recursive
  "Recursively delete a directory and its contents."
  [^File f]
  (when (.isDirectory f)
    (doseq [child (.listFiles f)]
      (delete-recursive child)))
  (.delete f))

(defmacro with-temp-events-dir
  "Execute body with a temporary events directory. Cleans up after."
  [[dir-sym] & body]
  `(let [~dir-sym (create-temp-dir)]
     (try
       ~@body
       (finally
         (delete-recursive (File. ~dir-sym))))))

(deftest flush-events!-test
  ;; Tests writing events to persistent storage.
  ;; Contracts:
  ;; - Writes session events to file
  ;; - Returns count of events written
  ;; - Creates directory if needed
  ;; - Only writes events for specified session
  (testing "flush-events!"
    (with-temp-events-dir [temp-dir]
      (events/clear-events!)

      (testing "writes session events to file"
        (let [e1 (events/create-event "sess-A" 42 :text-block "hello")
              e2 (events/create-event "sess-A" 42 :tool-use-block {:tool "Read"})]
          (events/add-event! e1)
          (events/add-event! e2)
          (let [count (events/flush-events! "sess-A" {:events-dir temp-dir})]
            (is (= 2 count))
            (is (.exists (File. (str temp-dir "/sess-A.edn")))))))

      (testing "returns 0 for session with no events"
        (is (= 0 (events/flush-events! "no-such" {:events-dir temp-dir}))))

      (testing "only writes events for specified session"
        (events/clear-events!)
        (events/add-event! (events/create-event "sess-X" 1 :text-block))
        (events/add-event! (events/create-event "sess-Y" 2 :text-block))
        (events/add-event! (events/create-event "sess-X" 1 :tool-use-block))
        (let [count (events/flush-events! "sess-X" {:events-dir temp-dir})]
          (is (= 2 count)))))))

(deftest load-session-events-test
  ;; Tests reading events from persistent storage.
  ;; Contracts:
  ;; - Returns nil for non-existent file
  ;; - Reads events with correct types and timestamps
  ;; - Validates loaded events
  (testing "load-session-events"
    (with-temp-events-dir [temp-dir]
      (events/clear-events!)

      (testing "returns nil for non-existent session"
        (is (nil? (events/load-session-events "no-file" {:events-dir temp-dir}))))

      (testing "reads events written by flush-events!"
        (let [e1 (events/create-event "round-trip" 100 :text-block "content1")
              e2 (events/create-event "round-trip" 100 :thinking-block "thought")]
          (events/add-event! e1)
          (events/add-event! e2)
          (events/flush-events! "round-trip" {:events-dir temp-dir})
          (events/clear-events!)
          (let [loaded (events/load-session-events "round-trip"
                                                   {:events-dir temp-dir})]
            (is (= 2 (count loaded)))
            (is (= "round-trip" (:session-id (first loaded))))
            (is (= :text-block (:type (first loaded))))
            (is (= "content1" (:content (first loaded))))
            (is (inst? (:timestamp (first loaded))))
            (is (= :thinking-block (:type (second loaded)))))))

      (testing "validates loaded events"
        ;; Event has valid timestamp format but invalid :type keyword
        (spit (str temp-dir "/invalid.edn")
              "{:timestamp \"2024-01-15T10:30:00Z\" :session-id \"x\" :story-id 1 :type :bad-type}\n")
        (is (thrown-with-msg? clojure.lang.ExceptionInfo
                              #"Invalid Event"
                              (events/load-session-events "invalid"
                                                          {:events-dir temp-dir})))))))

(deftest file-io-round-trip-test
  ;; Tests complete round-trip: create -> add -> flush -> clear -> load.
  ;; Contract: events survive persistence with full fidelity.
  (testing "file I/O round-trip"
    (with-temp-events-dir [temp-dir]
      (events/clear-events!)

      (testing "preserves all event fields through persistence"
        (let [original (events/create-event "fidelity" 999 :tool-result-block
                                            {:result "success" :code 0}
                                            {:task-id 555})]
          (events/add-event! original)
          (events/flush-events! "fidelity" {:events-dir temp-dir})
          (events/clear-events!)

          (let [[loaded] (events/load-session-events "fidelity"
                                                     {:events-dir temp-dir})]
            (is (= (:session-id original) (:session-id loaded)))
            (is (= (:story-id original) (:story-id loaded)))
            (is (= (:task-id original) (:task-id loaded)))
            (is (= (:type original) (:type loaded)))
            (is (= (:content original) (:content loaded)))
            (is (= (:timestamp original) (:timestamp loaded)))))))))

;;; SDK Event Callback Tests

(deftest make-event-callback-test
  ;; Tests make-event-callback returns correct structure and captures events.
  ;; Contracts:
  ;; - Returns map with :callback and :context-atom
  ;; - Context-atom contains story-id, task-id, and generated session-id
  ;; - Can provide custom initial session-id
  (testing "make-event-callback"
    (testing "returns callback and context-atom"
      (let [result (events/make-event-callback {:story-id 42})]
        (is (map? result))
        (is (fn? (:callback result)))
        (is (instance? clojure.lang.Atom (:context-atom result)))))

    (testing "context-atom contains story-id and generated session-id"
      (let [{:keys [context-atom]} (events/make-event-callback {:story-id 42})]
        (is (= 42 (:story-id @context-atom)))
        (is (string? (:session-id @context-atom)))
        (is (pos? (count (:session-id @context-atom))))))

    (testing "includes task-id in context when provided"
      (let [{:keys [context-atom]} (events/make-event-callback {:story-id 42
                                                                :task-id 100})]
        (is (= 100 (:task-id @context-atom)))))

    (testing "uses provided session-id"
      (let [{:keys [context-atom]} (events/make-event-callback
                                    {:story-id 42}
                                    {:session-id "custom-id"})]
        (is (= "custom-id" (:session-id @context-atom)))))))

(deftest event-callback-captures-blocks-test
  ;; Tests callback captures content blocks from AssistantMessage.
  ;; Contracts:
  ;; - Text blocks captured with :text-block type
  ;; - Thinking blocks captured with :thinking-block type
  ;; - Tool use blocks captured with :tool-use-block type
  ;; - Tool result blocks captured with :tool-result-block type
  (testing "event callback"
    (testing "captures text-block from AssistantMessage"
      (events/clear-events!)
      (let [{:keys [callback]} (events/make-event-callback
                                {:story-id 42}
                                {:session-id "test-sess"})
            mock-msg {:type :assistant-message
                      :content [{:type :text-block :text "Hello world"}]
                      :model "claude-test"}]
        (with-redefs [sdk/parse-message (constantly mock-msg)]
          (callback :dummy-py-msg))
        (let [captured (events/get-events)]
          (is (= 1 (count captured)))
          (is (= :text-block (:type (first captured))))
          (is (= "test-sess" (:session-id (first captured))))
          (is (= 42 (:story-id (first captured)))))))

    (testing "captures multiple block types"
      (events/clear-events!)
      (let [{:keys [callback]} (events/make-event-callback
                                {:story-id 42 :task-id 100}
                                {:session-id "multi-sess"})
            mock-msg {:type :assistant-message
                      :content [{:type :thinking-block :thinking "hmm"}
                                {:type :text-block :text "Hello"}
                                {:type :tool-use-block :id "t1" :name "Read"}
                                {:type :tool-result-block :tool-use-id "t1"}]
                      :model "claude-test"}]
        (with-redefs [sdk/parse-message (constantly mock-msg)]
          (callback :dummy-py-msg))
        (let [captured (events/get-events)]
          (is (= 4 (count captured)))
          (is (= [:thinking-block :text-block :tool-use-block :tool-result-block]
                 (mapv :type captured)))
          (is (every? #(= 100 (:task-id %)) captured)))))))

(deftest event-callback-result-message-test
  ;; Tests callback captures ResultMessage and updates session-id.
  ;; Contracts:
  ;; - ResultMessage captured with :result-message type
  ;; - Context-atom session-id updated from ResultMessage
  (testing "event callback"
    (testing "captures result-message"
      (events/clear-events!)
      (let [{:keys [callback]} (events/make-event-callback
                                {:story-id 42}
                                {:session-id "temp-id"})
            mock-msg {:type :result-message
                      :session-id "real-session-id"
                      :duration-ms 1000
                      :is-error false}]
        (with-redefs [sdk/parse-message (constantly mock-msg)]
          (callback :dummy-py-msg))
        (let [captured (events/get-events)]
          (is (= 1 (count captured)))
          (is (= :result-message (:type (first captured))))
          (is (= "real-session-id" (:session-id (first captured)))))))

    (testing "updates context-atom with real session-id"
      (events/clear-events!)
      (let [{:keys [callback context-atom]} (events/make-event-callback
                                             {:story-id 42}
                                             {:session-id "temp-id"})
            mock-msg {:type :result-message
                      :session-id "real-session-id"
                      :duration-ms 1000}]
        (is (= "temp-id" (:session-id @context-atom)))
        (with-redefs [sdk/parse-message (constantly mock-msg)]
          (callback :dummy-py-msg))
        (is (= "real-session-id" (:session-id @context-atom)))))))

(deftest event-callback-ignores-other-messages-test
  ;; Tests callback ignores non-capturable message types.
  ;; Contracts:
  ;; - UserMessage not captured
  ;; - SystemMessage not captured
  (testing "event callback"
    (testing "ignores UserMessage"
      (events/clear-events!)
      (let [{:keys [callback]} (events/make-event-callback {:story-id 42})]
        (with-redefs [sdk/parse-message (constantly {:type :user-message
                                                     :content "hello"})]
          (callback :dummy-py-msg))
        (is (empty? (events/get-events)))))

    (testing "ignores SystemMessage"
      (events/clear-events!)
      (let [{:keys [callback]} (events/make-event-callback {:story-id 42})]
        (with-redefs [sdk/parse-message (constantly {:type :system-message
                                                     :subtype "info"
                                                     :data {}})]
          (callback :dummy-py-msg))
        (is (empty? (events/get-events)))))))

(deftest event-callback-error-handling-test
  ;; Tests callback handles errors gracefully without propagating.
  ;; Contract: parse errors don't break the callback.
  (testing "event callback"
    (testing "handles parse errors gracefully"
      (events/clear-events!)
      (let [{:keys [callback]} (events/make-event-callback {:story-id 42})]
        (with-redefs [sdk/parse-message (fn [_] (throw (Exception. "parse err")))]
          ;; Should not throw
          (is (nil? (callback :dummy-py-msg))))
        (is (empty? (events/get-events)))))))
