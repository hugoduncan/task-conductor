(ns task-conductor.claude-cli-test.core-test
  "Tests for Claude CLI core functionality."
  (:require [clojure.test :refer [deftest is testing]]
            [task-conductor.claude-cli.core :as core]))

;; Tests that build-args correctly translates options maps to CLI argument vectors.
;; Contracts: always includes base flags, maps each option correctly, prompt goes last.

(def base-args
  ["claude" "--output-format" "stream-json" "--print" "conversation-summary"])

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
