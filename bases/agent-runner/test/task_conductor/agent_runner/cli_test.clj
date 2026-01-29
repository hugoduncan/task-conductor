(ns task-conductor.agent-runner.cli-test
  ;; Tests for CLI session creation and stream-json parsing.
  ;;
  ;; Contracts tested:
  ;; - parse-stream-json-line returns parsed JSON for valid input
  ;; - parse-stream-json-line returns nil for invalid/empty input
  ;; - map-stream-message-to-events extracts session-id from system/init
  ;; - map-stream-message-to-events maps assistant content blocks to events
  ;; - map-stream-message-to-events maps result messages to result-message events
  ;; - map-stream-message-to-events returns empty vector for unrecognized types
  ;; - create-session-via-cli returns session-id and result on success
  ;; - create-session-via-cli calls event-callback for each event
  ;; - create-session-via-cli flushes events after completion
  ;; - Throws :cli/timeout when process exceeds timeout
  ;; - Throws :cli/non-zero-exit when process fails
  ;; - Throws :cli/missing-session-id when no session_id in stream
  (:require
   [babashka.process :as p]
   [clojure.data.json :as json]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [task-conductor.agent-runner.cli :as cli]
   [task-conductor.agent-runner.events :as events])
  (:import
   [java.io ByteArrayInputStream]))

;;; Test Fixtures

(defn clear-events-fixture [f]
  (events/clear-events!)
  (f)
  (events/clear-events!))

(use-fixtures :each clear-events-fixture)

;;; Test Utilities

(defn- string->stream
  "Convert a string to an InputStream."
  [^String s]
  (ByteArrayInputStream. (.getBytes s "UTF-8")))

(defn- stream-json-output
  "Build stream-json output from a sequence of JSON maps.
   Each map is serialized to JSON and separated by newlines."
  [& messages]
  (str/join "\n" (map json/write-str messages)))

(defn- mock-java-process
  "Create a mock java.lang.Process with a pid method."
  []
  (proxy [java.lang.Process] []
    (pid [] 12345)))

(defn mock-process
  "Create a mock process that mimics babashka.process output.

   Returns a map-like object with :out, :err as InputStreams, :proc as mock
   Process, and implements IDeref/IBlockingDeref for exit code.

   Options:
   - :exit - exit code (default 0)
   - :out - stdout string (default \"\")
   - :err - stderr string (default \"\")
   - :delay-ms - delay before returning (for timeout tests)"
  [{:keys [exit out err delay-ms]
    :or {exit 0 out "" err ""}}]
  (let [deref-result {:exit exit}
        out-stream (string->stream out)
        err-stream (string->stream err)
        proc (mock-java-process)]
    (reify
      clojure.lang.ILookup
      (valAt [_ k] (case k :out out-stream :err err-stream :proc proc nil))
      (valAt [this k not-found] (or (.valAt this k) not-found))
      clojure.lang.IDeref
      (deref [_]
        (when delay-ms (Thread/sleep delay-ms))
        deref-result)
      clojure.lang.IBlockingDeref
      (deref [_ timeout-ms timeout-val]
        (if (and delay-ms (> delay-ms timeout-ms))
          timeout-val
          (do
            (when delay-ms (Thread/sleep (min delay-ms timeout-ms)))
            deref-result))))))

;;; parse-stream-json-line Tests

(deftest parse-stream-json-line-valid-json-test
  (testing "parse-stream-json-line"
    (testing "parses valid JSON object"
      (is (= {:type "system" :subtype "init"}
             (cli/parse-stream-json-line "{\"type\":\"system\",\"subtype\":\"init\"}"))))

    (testing "parses JSON with nested structures"
      (is (= {:type "assistant"
              :message {:content [{:type "text" :text "Hello"}]}}
             (cli/parse-stream-json-line
              "{\"type\":\"assistant\",\"message\":{\"content\":[{\"type\":\"text\",\"text\":\"Hello\"}]}}"))))

    (testing "parses JSON with leading whitespace"
      (is (= {:key "value"}
             (cli/parse-stream-json-line "  {\"key\":\"value\"}"))))))

(deftest parse-stream-json-line-invalid-input-test
  (testing "parse-stream-json-line"
    (testing "returns nil for nil input"
      (is (nil? (cli/parse-stream-json-line nil))))

    (testing "returns nil for empty string"
      (is (nil? (cli/parse-stream-json-line ""))))

    (testing "returns nil for blank string with spaces"
      (is (nil? (cli/parse-stream-json-line "   "))))

    (testing "returns nil for non-JSON text"
      (is (nil? (cli/parse-stream-json-line "not json at all"))))

    (testing "returns nil for text starting with non-brace"
      (is (nil? (cli/parse-stream-json-line "[1,2,3]"))))

    (testing "returns nil for invalid JSON and logs warning"
      (is (nil? (cli/parse-stream-json-line "{invalid json"))))))

;;; map-stream-message-to-events Tests

(deftest map-stream-message-to-events-system-init-test
  ;; Tests that system/init messages extract session-id metadata.
  (testing "map-stream-message-to-events"
    (testing "given system/init message"
      (testing "returns session-id metadata"
        (is (= {:session-id "abc-123"}
               (cli/map-stream-message-to-events
                {:type "system"
                 :subtype "init"
                 :session_id "abc-123"})))))

    (testing "given system message with other subtype"
      (testing "returns empty vector"
        (is (= []
               (cli/map-stream-message-to-events
                {:type "system"
                 :subtype "other"})))))))

(deftest map-stream-message-to-events-assistant-test
  ;; Tests that assistant messages map content blocks to events.
  (testing "map-stream-message-to-events"
    (testing "given assistant message with text block"
      (testing "returns text-block event"
        (is (= [{:type :text-block :text "Hello world"}]
               (cli/map-stream-message-to-events
                {:type "assistant"
                 :message {:content [{:type "text"
                                      :text "Hello world"}]}})))))

    (testing "given assistant message with tool_use block"
      (testing "returns tool-use-block event"
        (is (= [{:type :tool-use-block
                 :id "tool-1"
                 :name "Read"
                 :input {:path "/tmp/foo"}}]
               (cli/map-stream-message-to-events
                {:type "assistant"
                 :message {:content [{:type "tool_use"
                                      :id "tool-1"
                                      :name "Read"
                                      :input {:path "/tmp/foo"}}]}})))))

    (testing "given assistant message with thinking block"
      (testing "returns thinking-block event"
        (is (= [{:type :thinking-block :thinking "Let me think"}]
               (cli/map-stream-message-to-events
                {:type "assistant"
                 :message {:content [{:type "thinking"
                                      :thinking "Let me think"}]}})))))

    (testing "given assistant message with multiple blocks"
      (testing "returns one event per block"
        (is (= [{:type :thinking-block :thinking "hmm"}
                {:type :text-block :text "Hello"}
                {:type :tool-use-block :id "t1" :name "Bash" :input {}}]
               (cli/map-stream-message-to-events
                {:type "assistant"
                 :message {:content [{:type "thinking" :thinking "hmm"}
                                     {:type "text" :text "Hello"}
                                     {:type "tool_use"
                                      :id "t1"
                                      :name "Bash"
                                      :input {}}]}})))))

    (testing "given assistant message with unrecognized block type"
      (testing "filters out the unrecognized block"
        (is (= [{:type :text-block :text "Hello"}]
               (cli/map-stream-message-to-events
                {:type "assistant"
                 :message {:content [{:type "unknown" :data "ignored"}
                                     {:type "text" :text "Hello"}]}})))))))

(deftest map-stream-message-to-events-result-test
  ;; Tests that result messages map to result-message events.
  (testing "map-stream-message-to-events"
    (testing "given result message with usage"
      (testing "returns result-message event"
        (is (= [{:type :result-message
                 :usage {:input_tokens 100
                         :output_tokens 50}}]
               (cli/map-stream-message-to-events
                {:type "result"
                 :subtype "success"
                 :usage {:input_tokens 100
                         :output_tokens 50}})))))

    (testing "given result message without usage"
      (testing "returns result-message with nil usage"
        (is (= [{:type :result-message :usage nil}]
               (cli/map-stream-message-to-events
                {:type "result"
                 :subtype "success"})))))))

(deftest map-stream-message-to-events-unrecognized-test
  ;; Tests that unrecognized message types return empty vector.
  (testing "map-stream-message-to-events"
    (testing "given unrecognized message type"
      (testing "returns empty vector"
        (is (= []
               (cli/map-stream-message-to-events
                {:type "unknown"
                 :data "something"})))))))

;;; Success Tests

(deftest create-session-via-cli-success-test
  ;; Tests successful CLI session creation with stream-json output.
  (testing "create-session-via-cli"
    (testing "given valid stream-json output"
      (testing "returns session-id and result"
        (let [stream-out (stream-json-output
                          {:type "system" :subtype "init" :session_id "abc-123"}
                          {:type "assistant"
                           :message {:content [{:type "text" :text "Hello"}]}}
                          {:type "result" :subtype "success"
                           :usage {:input_tokens 10 :output_tokens 5}})]
          (with-redefs [p/process (fn [_opts & _args]
                                    (mock-process {:out stream-out}))]
            (let [result (cli/create-session-via-cli
                          {:working-dir "/tmp"
                           :prompt "Hello"})]
              (is (= "abc-123" (:session-id result)))
              (is (= {:type "result"
                      :subtype "success"
                      :usage {:input_tokens 10 :output_tokens 5}}
                     (:result result))))))))

    (testing "passes correct arguments to claude CLI"
      (let [captured-args (atom nil)
            stream-out (stream-json-output
                        {:type "system" :subtype "init" :session_id "x"}
                        {:type "result" :subtype "success"})]
        (with-redefs [p/process (fn [opts & args]
                                  (reset! captured-args {:opts opts :args args})
                                  (mock-process {:out stream-out}))]
          (cli/create-session-via-cli {:working-dir "/my/dir"
                                       :prompt "Test prompt"})
          (is (= "/my/dir" (get-in @captured-args [:opts :dir])))
          (is (= ["claude" "--print" "--verbose" "--output-format" "stream-json"
                  "-p" "Test prompt"]
                 (:args @captured-args))))))

    (testing "uses default timeout when not specified"
      (let [deref-timeout (atom nil)
            stream-out (stream-json-output
                        {:type "system" :subtype "init" :session_id "x"}
                        {:type "result" :subtype "success"})
            out-stream (string->stream stream-out)
            err-stream (string->stream "")
            proc (mock-java-process)]
        (with-redefs [p/process (fn [_ & _]
                                  (reify
                                    clojure.lang.ILookup
                                    (valAt [_ k] (case k :out out-stream :err err-stream :proc proc nil))
                                    (valAt [this k nf] (or (.valAt this k) nf))
                                    clojure.lang.IBlockingDeref
                                    (deref [_ timeout-ms _timeout-val]
                                      (reset! deref-timeout timeout-ms)
                                      {:exit 0})))]
          (cli/create-session-via-cli {:working-dir "/tmp" :prompt "Hi"})
          (is (= 300000 @deref-timeout)))))

    (testing "uses custom timeout when specified"
      (let [deref-timeout (atom nil)
            stream-out (stream-json-output
                        {:type "system" :subtype "init" :session_id "x"}
                        {:type "result" :subtype "success"})
            out-stream (string->stream stream-out)
            err-stream (string->stream "")
            proc (mock-java-process)]
        (with-redefs [p/process (fn [_ & _]
                                  (reify
                                    clojure.lang.ILookup
                                    (valAt [_ k] (case k :out out-stream :err err-stream :proc proc nil))
                                    (valAt [this k nf] (or (.valAt this k) nf))
                                    clojure.lang.IBlockingDeref
                                    (deref [_ timeout-ms _timeout-val]
                                      (reset! deref-timeout timeout-ms)
                                      {:exit 0})))]
          (cli/create-session-via-cli {:working-dir "/tmp"
                                       :prompt "Hi"
                                       :timeout-ms 5000})
          (is (= 5000 @deref-timeout)))))))

(deftest create-session-via-cli-event-callback-test
  ;; Tests that event-callback is called for each event during execution.
  (testing "create-session-via-cli"
    (testing "given event-callback option"
      (testing "calls callback for each event"
        (let [captured-events (atom [])
              stream-out (stream-json-output
                          {:type "system" :subtype "init" :session_id "ses-1"}
                          {:type "assistant"
                           :message {:content [{:type "text" :text "Hi"}
                                               {:type "tool_use" :id "t1"
                                                :name "Bash" :input {}}]}}
                          {:type "result" :subtype "success"
                           :usage {:input_tokens 10 :output_tokens 5}})]
          (with-redefs [p/process (fn [_ & _]
                                    (mock-process {:out stream-out}))]
            (cli/create-session-via-cli
             {:working-dir "/tmp"
              :prompt "Hi"
              :event-callback #(swap! captured-events conj %)})
            (is (= 4 (count @captured-events))
                "should emit session-id-update plus 3 content events")
            (is (= :session-id-update (:type (first @captured-events)))
                "first event should be session-id-update")
            (is (= "ses-1" (:session-id (first @captured-events)))
                "session-id-update should contain the session-id")
            (is (= :text-block (:type (second @captured-events))))
            (is (= :tool-use-block (:type (nth @captured-events 2))))
            (is (= :result-message (:type (nth @captured-events 3))))))))))

(deftest create-session-via-cli-flush-events-test
  ;; Tests that events are flushed to persistent storage after completion.
  (testing "create-session-via-cli"
    (testing "flushes events after successful completion"
      (let [flushed-session-id (atom nil)
            stream-out (stream-json-output
                        {:type "system" :subtype "init" :session_id "flush-test"}
                        {:type "result" :subtype "success"})]
        (with-redefs [p/process (fn [_ & _]
                                  (mock-process {:out stream-out}))
                      events/flush-events! (fn [sid]
                                             (reset! flushed-session-id sid)
                                             0)]
          (cli/create-session-via-cli {:working-dir "/tmp" :prompt "Hi"})
          (is (= "flush-test" @flushed-session-id)))))))

;;; Error Tests

(deftest create-session-via-cli-timeout-test
  (testing "create-session-via-cli"
    (testing "throws :cli/timeout when process times out"
      (let [destroyed? (atom false)]
        (with-redefs [p/process (fn [_ & _]
                                  (mock-process {:delay-ms 10000}))
                      p/destroy-tree (fn [_] (reset! destroyed? true))]
          (let [ex (try
                     (cli/create-session-via-cli {:working-dir "/tmp"
                                                  :prompt "Hi"
                                                  :timeout-ms 10})
                     nil
                     (catch Exception e e))]
            (is (some? ex))
            (is (= :cli/timeout (:type (ex-data ex))))
            (is (= 10 (:timeout-ms (ex-data ex))))
            (is @destroyed? "should destroy process on timeout")))))))

(deftest create-session-via-cli-non-zero-exit-test
  ;; Tests that non-zero exit code throws :cli/non-zero-exit error.
  (testing "create-session-via-cli"
    (testing "throws :cli/non-zero-exit when process fails"
      (with-redefs [p/process (fn [_ & _]
                                (mock-process {:exit 1
                                               :out ""
                                               :err "Error occurred"}))]
        (let [ex (try
                   (cli/create-session-via-cli {:working-dir "/tmp"
                                                :prompt "Hi"})
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/non-zero-exit (:type (ex-data ex))))
          (is (= 1 (:exit-code (ex-data ex)))))))))

(deftest create-session-via-cli-missing-session-id-test
  ;; Tests that missing session-id in stream throws :cli/missing-session-id.
  (testing "create-session-via-cli"
    (testing "given stream without system/init message"
      (testing "throws :cli/missing-session-id"
        (let [stream-out (stream-json-output
                          {:type "assistant"
                           :message {:content [{:type "text" :text "Hi"}]}}
                          {:type "result" :subtype "success"})]
          (with-redefs [p/process (fn [_ & _]
                                    (mock-process {:out stream-out}))]
            (let [ex (try
                       (cli/create-session-via-cli {:working-dir "/tmp"
                                                    :prompt "Hi"})
                       nil
                       (catch Exception e e))]
              (is (some? ex))
              (is (= :cli/missing-session-id (:type (ex-data ex)))))))))

    (testing "given empty stream output"
      (testing "throws :cli/missing-session-id"
        (with-redefs [p/process (fn [_ & _]
                                  (mock-process {:out ""}))]
          (let [ex (try
                     (cli/create-session-via-cli {:working-dir "/tmp"
                                                  :prompt "Hi"})
                     nil
                     (catch Exception e e))]
            (is (some? ex))
            (is (= :cli/missing-session-id (:type (ex-data ex))))))))))
