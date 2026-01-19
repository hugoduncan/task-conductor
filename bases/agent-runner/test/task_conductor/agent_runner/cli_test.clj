(ns task-conductor.agent-runner.cli-test
  ;; Tests for CLI session creation.
  ;;
  ;; Contracts tested:
  ;; - create-session-via-cli returns session-id and response on success
  ;; - Throws :cli/timeout when process exceeds timeout
  ;; - Throws :cli/non-zero-exit when process fails
  ;; - Throws :cli/parse-error when output is not valid JSON
  ;; - Throws :cli/missing-session-id when session_id absent
  (:require
   [babashka.process :as p]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.cli :as cli])
  (:import
   [java.io ByteArrayInputStream]))

;;; Test Utilities

(defn- string->stream
  "Convert a string to an InputStream."
  [^String s]
  (ByteArrayInputStream. (.getBytes s "UTF-8")))

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

;;; Success Tests

(deftest create-session-via-cli-success-test
  (testing "create-session-via-cli"
    (testing "returns session-id and response on success"
      (let [json-response "{\"session_id\":\"abc-123\",\"result\":\"ok\"}"
            captured-args (atom nil)]
        (with-redefs [p/process (fn [opts & args]
                                  (reset! captured-args {:opts opts :args args})
                                  (mock-process {:out json-response}))]
          (let [result (cli/create-session-via-cli
                        {:working-dir "/tmp"
                         :prompt "Hello"})]
            (is (= "abc-123" (:session-id result)))
            (is (= {:session_id "abc-123" :result "ok"} (:response result)))))))

    (testing "passes correct arguments to claude CLI"
      (let [captured-args (atom nil)]
        (with-redefs [p/process (fn [opts & args]
                                  (reset! captured-args {:opts opts :args args})
                                  (mock-process {:out "{\"session_id\":\"x\"}"}))]
          (cli/create-session-via-cli {:working-dir "/my/dir"
                                       :prompt "Test prompt"})
          (is (= "/my/dir" (get-in @captured-args [:opts :dir])))
          (is (= ["claude" "--print" "--output-format" "json"
                  "-p" "Test prompt"]
                 (:args @captured-args))))))

    (testing "uses default timeout when not specified"
      (let [deref-timeout (atom nil)
            out-stream (string->stream "{\"session_id\":\"x\"}")
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
            out-stream (string->stream "{\"session_id\":\"x\"}")
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
          (is (= 1 (:exit-code (ex-data ex))))
          (is (= "Error occurred\n" (:stderr (ex-data ex)))))))))

(deftest create-session-via-cli-parse-error-test
  (testing "create-session-via-cli"
    (testing "throws :cli/parse-error when output is not valid JSON"
      (with-redefs [p/process (fn [_ & _]
                                (mock-process {:out "not json"}))]
        (let [ex (try
                   (cli/create-session-via-cli {:working-dir "/tmp"
                                                :prompt "Hi"})
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/parse-error (:type (ex-data ex))))
          (is (= "not json\n" (:output (ex-data ex)))))))

    (testing "throws :cli/parse-error when output is truncated JSON"
      (with-redefs [p/process (fn [_ & _]
                                (mock-process {:out "{\"session_id\":"}))]
        (let [ex (try
                   (cli/create-session-via-cli {:working-dir "/tmp"
                                                :prompt "Hi"})
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/parse-error (:type (ex-data ex)))))))))

(deftest create-session-via-cli-missing-session-id-test
  (testing "create-session-via-cli"
    (testing "throws :cli/missing-session-id when session_id absent"
      (with-redefs [p/process (fn [_ & _]
                                (mock-process {:out "{\"result\":\"ok\"}"}))]
        (let [ex (try
                   (cli/create-session-via-cli {:working-dir "/tmp"
                                                :prompt "Hi"})
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/missing-session-id (:type (ex-data ex))))
          (is (= {:result "ok"} (:response (ex-data ex)))))))

    (testing "throws :cli/missing-session-id when session_id is null"
      (with-redefs [p/process (fn [_ & _]
                                (mock-process {:out "{\"session_id\":null}"}))]
        (let [ex (try
                   (cli/create-session-via-cli {:working-dir "/tmp"
                                                :prompt "Hi"})
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/missing-session-id (:type (ex-data ex)))))))))
