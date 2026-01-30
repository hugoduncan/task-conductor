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
