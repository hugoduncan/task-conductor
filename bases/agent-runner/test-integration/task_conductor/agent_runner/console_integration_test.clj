(ns task-conductor.agent-runner.console-integration-test
  ;; Integration tests for babashka.process API used by launch-cli-resume.
  ;; These tests verify that the process launching pattern works with real
  ;; commands, catching any babashka.process API changes.
  ;;
  ;; Contracts tested:
  ;; - p/process with {:inherit true} returns a derefable process
  ;; - Dereferencing yields map with :exit key
  ;; - Exit code reflects actual command result
  ;;
  ;; Run with: clj -M:test :integration
  ;; Skip with: SKIP_INTEGRATION_TESTS=1
  (:require
   [babashka.process :as p]
   [clojure.test :refer [deftest is testing]]))

(defn- skip-integration-tests?
  "Check if integration tests should be skipped.
   Set SKIP_INTEGRATION_TESTS=1 to skip explicitly."
  []
  (some? (System/getenv "SKIP_INTEGRATION_TESTS")))

(defmacro with-skip-check
  "Execute body unless SKIP_INTEGRATION_TESTS is set."
  [& body]
  `(if (skip-integration-tests?)
     (testing "skipped (SKIP_INTEGRATION_TESTS set)"
       (is true))
     (do ~@body)))

(deftest babashka-process-api-test
  ;; Tests the babashka.process API pattern used by launch-cli-resume.
  ;; Uses simple shell commands to verify the API contract without
  ;; requiring claude CLI or authentication.
  (with-skip-check
    (testing "babashka.process API"
      (testing "with {:inherit true} option"
        (testing "returns exit code 0 for successful command"
          (let [proc (p/process ["true"] {:inherit true})
                result @proc]
            (is (map? result)
                "dereferenced process should be a map")
            (is (contains? result :exit)
                "result should contain :exit key")
            (is (= 0 (:exit result))
                "true command should exit with 0")))

        (testing "returns non-zero exit code for failing command"
          (let [proc (p/process ["false"] {:inherit true})
                result @proc]
            (is (= 1 (:exit result))
                "false command should exit with 1")))

        (testing "propagates actual exit code"
          (let [proc (p/process ["sh" "-c" "exit 42"] {:inherit true})
                result @proc]
            (is (= 42 (:exit result))
                "should return actual exit code from command")))))))
