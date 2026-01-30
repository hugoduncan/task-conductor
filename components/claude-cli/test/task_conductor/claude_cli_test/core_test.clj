(ns task-conductor.claude-cli-test.core-test
  "Tests for Claude CLI core functionality."
  (:require [clojure.test :refer [deftest is testing]]
            [task-conductor.claude-cli.core :as core]))

;; Tests CLI argument building and process management.

(deftest build-args-test
  (testing "build-args"
    (testing "returns empty vector for empty opts"
      (is (= [] (core/build-args {}))))))
