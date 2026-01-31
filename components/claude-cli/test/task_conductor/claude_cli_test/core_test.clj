(ns task-conductor.claude-cli-test.core-test
  "Tests for Claude CLI core functionality."
  (:require [clojure.test :refer [deftest is testing]]
            [task-conductor.claude-cli.core :as core]))

;; Tests that build-args correctly translates options maps to CLI argument vectors.
;; Contracts: always includes base flags, maps each option correctly, prompt goes last.

(def base-args
  ["claude" "--output-format" "stream-json" "--verbose"
   "--print" "conversation-summary"])

(deftest build-args-test
  (testing "build-args"
    (testing "with empty options"
      (testing "includes only base flags"
        (is (= base-args (core/build-args {})))))

    (testing "with :prompt"
      (testing "appends prompt as last argument"
        (is (= (conj base-args "hello")
               (core/build-args {:prompt "hello"})))))

    (testing "with :model"
      (testing "adds --model flag"
        (is (= (into base-args ["--model" "sonnet"])
               (core/build-args {:model "sonnet"})))))

    (testing "with :allowed-tools"
      (testing "adds --allowedTools with comma-separated values"
        (is (= (into base-args ["--allowedTools" "Bash,Read"])
               (core/build-args {:allowed-tools ["Bash" "Read"]}))))
      (testing "handles single tool"
        (is (= (into base-args ["--allowedTools" "Bash"])
               (core/build-args {:allowed-tools ["Bash"]}))))
      (testing "ignores empty vector"
        (is (= base-args (core/build-args {:allowed-tools []})))))

    (testing "with :disallowed-tools"
      (testing "adds --disallowedTools with comma-separated values"
        (is (= (into base-args ["--disallowedTools" "Write,Edit"])
               (core/build-args {:disallowed-tools ["Write" "Edit"]}))))
      (testing "ignores empty vector"
        (is (= base-args (core/build-args {:disallowed-tools []})))))

    (testing "with :max-turns"
      (testing "adds --max-turns as string"
        (is (= (into base-args ["--max-turns" "10"])
               (core/build-args {:max-turns 10})))))

    (testing "with :mcp-config"
      (testing "adds --mcp-config flag"
        (is (= (into base-args ["--mcp-config" "/path/to/config.json"])
               (core/build-args {:mcp-config "/path/to/config.json"})))))

    (testing "with all options"
      (testing "includes all flags in correct order with prompt last"
        (is (= (-> base-args
                   (into ["--model" "opus"])
                   (into ["--allowedTools" "Bash,Read"])
                   (into ["--disallowedTools" "Write"])
                   (into ["--max-turns" "5"])
                   (into ["--mcp-config" "/cfg.json"])
                   (conj "do something"))
               (core/build-args {:prompt "do something"
                                 :model "opus"
                                 :allowed-tools ["Bash" "Read"]
                                 :disallowed-tools ["Write"]
                                 :max-turns 5
                                 :mcp-config "/cfg.json"})))))))

;;; invoke-process tests

;; Tests that invoke-process correctly manages process lifecycle, parses JSON output,
;; handles callbacks, and respects timeout. Uses mock bash commands instead of real Claude CLI.

(deftest invoke-process-test
  (testing "invoke-process"
    (testing "with successful JSON output"
      (testing "parses events and returns exit code 0"
        (let [lines (atom [])
              events (atom [])
              {:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c"
                                                 "echo '{\"type\":\"start\"}'; echo '{\"type\":\"end\"}'"]
                                         :on-line #(swap! lines conj %)
                                         :on-event #(swap! events conj %)})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result) "should complete within timeout")
          (is (= 0 (:exit-code result)))
          (is (= [{:type "start"} {:type "end"}] (:events result)))
          (is (= ["{\"type\":\"start\"}" "{\"type\":\"end\"}"] @lines))
          (is (= [{:type "start"} {:type "end"}] @events)))))

    (testing "with non-zero exit code"
      (testing "returns exit code in result without throwing"
        (let [{:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c" "exit 42"]})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result))
          (is (= 42 (:exit-code result)))
          (is (= [] (:events result))))))

    (testing "with invalid JSON line"
      (testing "includes parse-error event"
        (let [{:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c"
                                                 "echo 'not json'; echo '{\"ok\":true}'"]})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result))
          (is (= 0 (:exit-code result)))
          (is (= [{:type "parse-error" :line "not json"}
                  {:ok true}]
                 (:events result))))))

    (testing "with timeout"
      (testing "returns timeout error and kills process"
        (let [{:keys [process result-promise]} (core/invoke-process
                                                {:_args ["bash" "-c" "sleep 10"]
                                                 :timeout 100})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result) "promise should be delivered")
          (is (= :timeout (:error result)))
          (is (nil? (:exit-code result)))
          ;; Verify process was destroyed
          (is (not (.isAlive (:proc process)))))))

    (testing "with :dir option"
      (testing "runs process in specified directory"
        (let [{:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c" "echo \"{\\\"cwd\\\":\\\"$(pwd)\\\"}\""]
                                         :dir "/tmp"})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result))
          (is (= 0 (:exit-code result)))
          ;; /tmp may resolve to /private/tmp on macOS
          (is (re-find #"/tmp$" (:cwd (first (:events result))))))))))

;;; cancel! tests

;; Tests that cancel! correctly terminates a running process and handles
;; already-terminated processes gracefully.

(deftest cancel!-test
  (testing "cancel!"
    (testing "on a running process"
      (testing "returns true and terminates the process"
        (let [start-time (System/currentTimeMillis)
              handle (core/invoke-process {:_args ["bash" "-c" "sleep 10"]})
              ;; Give process time to start
              _ (Thread/sleep 50)
              cancelled (core/cancel! handle)
              result (deref (:result-promise handle) 5000 :timeout)
              elapsed (- (System/currentTimeMillis) start-time)]
          (is (true? cancelled) "should return true when killing running process")
          (is (not= :timeout result) "promise should be delivered")
          (is (< elapsed 5000) "should terminate well before sleep finishes"))))

    (testing "on an already-terminated process"
      (testing "returns false"
        (let [handle (core/invoke-process {:_args ["bash" "-c" "exit 0"]})
              _ (deref (:result-promise handle) 5000 :timeout)
              ;; Process has already finished
              cancelled (core/cancel! handle)]
          (is (false? cancelled) "should return false for terminated process"))))))

;;; callback exception tests

;; Tests that callback exceptions don't interrupt processing.
;; Contracts: exceptions are recorded as callback-error events, processing continues.

;;; extract-session-id tests

;; Tests that extract-session-id correctly finds session_id from events.
;; Contracts: returns first session_id found, nil if none present.

(deftest extract-session-id-test
  (testing "extract-session-id"
    (testing "with no events"
      (testing "returns nil"
        (is (nil? (core/extract-session-id [])))))

    (testing "with events lacking session_id"
      (testing "returns nil"
        (is (nil? (core/extract-session-id [{:type "start"}
                                            {:type "end"}])))))

    (testing "with session_id in first event"
      (testing "returns the session_id"
        (is (= "abc-123"
               (core/extract-session-id [{:type "init"
                                          :session_id "abc-123"}
                                         {:type "end"}])))))

    (testing "with session_id in later event"
      (testing "returns the first session_id found"
        (is (= "xyz-789"
               (core/extract-session-id [{:type "start"}
                                         {:type "init"
                                          :session_id "xyz-789"}
                                         {:type "end"}])))))

    (testing "with multiple session_ids"
      (testing "returns the first one"
        (is (= "first-id"
               (core/extract-session-id [{:session_id "first-id"}
                                         {:session_id "second-id"}])))))))

;;; invoke-process session-id extraction tests

;; Tests that invoke-process extracts and returns session-id from events.

(deftest invoke-process-session-id-test
  (testing "invoke-process session-id extraction"
    (testing "with events containing session_id"
      (testing "includes :session-id in result"
        (let [{:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c"
                                                 "echo '{\"type\":\"init\",\"session_id\":\"sess-42\"}'"]})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result))
          (is (= "sess-42" (:session-id result))))))

    (testing "with no session_id in events"
      (testing "returns nil for :session-id"
        (let [{:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c"
                                                 "echo '{\"type\":\"error\"}'"]})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result))
          (is (nil? (:session-id result))))))

    (testing "with empty output"
      (testing "returns nil for :session-id"
        (let [{:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c" "exit 1"]})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result))
          (is (nil? (:session-id result))))))))

;;; callback exception tests

;; Tests that callback exceptions don't interrupt processing.
;; Contracts: exceptions are recorded as callback-error events, processing continues.

(deftest callback-exception-test
  (testing "invoke-process with callback exceptions"
    (testing "when on-line throws"
      (testing "records error and continues processing"
        (let [events-received (atom [])
              {:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c"
                                                 "echo '{\"n\":1}'; echo '{\"n\":2}'"]
                                         :on-line (fn [_] (throw (ex-info "line boom" {})))
                                         :on-event #(swap! events-received conj %)})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result) "should complete")
          (is (= 0 (:exit-code result)))
          ;; Both events should be captured despite callback errors
          (is (= [{:type "callback-error" :callback :on-line :error "line boom"}
                  {:n 1}
                  {:type "callback-error" :callback :on-line :error "line boom"}
                  {:n 2}]
                 (:events result))))))

    (testing "when on-event throws"
      (testing "records error and continues processing"
        (let [lines-received (atom [])
              {:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c"
                                                 "echo '{\"n\":1}'; echo '{\"n\":2}'"]
                                         :on-line #(swap! lines-received conj %)
                                         :on-event (fn [_] (throw (ex-info "event boom" {})))})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result) "should complete")
          (is (= 0 (:exit-code result)))
          ;; Both lines should be received
          (is (= ["{\"n\":1}" "{\"n\":2}"] @lines-received))
          ;; Events include callback errors
          (is (= [{:n 1}
                  {:type "callback-error" :callback :on-event :error "event boom"}
                  {:n 2}
                  {:type "callback-error" :callback :on-event :error "event boom"}]
                 (:events result))))))

    (testing "when both callbacks throw"
      (testing "records errors from both and completes"
        (let [{:keys [result-promise]} (core/invoke-process
                                        {:_args ["bash" "-c" "echo '{\"n\":1}'"]
                                         :on-line (fn [_] (throw (ex-info "line err" {})))
                                         :on-event (fn [_] (throw (ex-info "event err" {})))})
              result (deref result-promise 5000 :timeout)]
          (is (not= :timeout result) "should complete")
          (is (= 0 (:exit-code result)))
          (is (= [{:type "callback-error" :callback :on-line :error "line err"}
                  {:n 1}
                  {:type "callback-error" :callback :on-event :error "event err"}]
                 (:events result))))))))
