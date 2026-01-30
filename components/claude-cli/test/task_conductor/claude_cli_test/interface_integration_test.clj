(ns task-conductor.claude-cli-test.interface-integration-test
  "Integration tests for Claude CLI interface using real Claude CLI.

  These tests require the Claude CLI to be installed and configured.
  They are excluded from default test runs.

  To run integration tests:
    clj -M:test integration

  To run all tests including integration:
    clj -M:test unit integration"
  (:require [clojure.test :refer [deftest is testing]]
            [task-conductor.claude-cli.interface :as cli]))

;; Tests verify the full integration stack: process spawning, JSON streaming,
;; event parsing, callbacks, and process lifecycle management.

(deftest ^:integration invoke-success-test
  (testing "invoke"
    (testing "with real Claude CLI"
      (testing "streams events and returns exit code 0"
        (let [events (atom [])
              lines (atom [])
              {:keys [result-promise]} (cli/invoke
                                        {:prompt "respond with exactly: hello"
                                         :max-turns 1
                                         :timeout 30000
                                         :on-line #(swap! lines conj %)
                                         :on-event #(swap! events conj %)})
              result (deref result-promise 35000 :timeout)]
          (is (not= :timeout result) "should complete within timeout")
          (is (= 0 (:exit-code result)) "should exit successfully")
          (is (seq (:events result)) "should have parsed events")
          (is (seq @lines) "on-line callback should be called")
          (is (seq @events) "on-event callback should be called")
          (is (some #(= "assistant" (:type %)) (:events result))
              "should have assistant message events"))))))

(deftest ^:integration cancel-test
  (testing "cancel!"
    (testing "with real Claude CLI"
      (testing "terminates a running invocation quickly"
        (let [start-time (System/currentTimeMillis)
              handle (cli/invoke {:prompt "count from 1 to 1000"
                                  :max-turns 5
                                  :timeout 60000})
              _ (Thread/sleep 500)
              cancelled (cli/cancel! handle)
              result (deref (:result-promise handle) 5000 :timeout)
              elapsed (- (System/currentTimeMillis) start-time)]
          (is (true? cancelled) "should return true for running process")
          (is (not= :timeout result) "promise should be delivered")
          (is (< elapsed 10000) "should terminate quickly after cancel"))))))
