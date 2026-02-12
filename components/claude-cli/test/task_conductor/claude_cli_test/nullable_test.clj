(ns task-conductor.claude-cli-test.nullable-test
  "Tests for claude-cli Nullable infrastructure.
  Verifies configurable responses, invocation tracking, and error simulation."
  (:require [clojure.test :refer [deftest is testing]]
            [task-conductor.claude-cli.interface :as cli]))

;; Tests that the Nullable provides configurable responses and tracks invocations
;; without spawning real processes. Contracts: default responses, custom config,
;; error simulation, invocation tracking.

(deftest make-nullable-test
  (testing "make-nullable"
    (testing "with no config"
      (testing "creates nullable with empty config"
        (let [nullable (cli/make-nullable)]
          (is (= {} (:config nullable)))
          (is (= [] @(:invocations nullable))))))

    (testing "with config"
      (testing "stores provided config"
        (let [config {:exit-code 1 :events [{:type "err"}]}
              nullable (cli/make-nullable config)]
          (is (= config (:config nullable))))))))

(deftest with-nullable-claude-cli-test
  (testing "with-nullable-claude-cli"
    (testing "with default config"
      (testing "returns default success result"
        (let [nullable (cli/make-nullable)
              {:keys [result-promise process]} (cli/with-nullable-claude-cli nullable
                                                 (cli/invoke {:prompt "test"}))]
          (is (nil? process) "process should be nil for nullable")
          (let [result @result-promise]
            (is (= 0 (:exit-code result)))
            (is (= [] (:events result)))
            (is (= "test-session" (:session-id result)))))))

    (testing "with custom exit-code"
      (testing "returns configured exit-code"
        (let [nullable (cli/make-nullable {:exit-code 42})
              {:keys [result-promise]} (cli/with-nullable-claude-cli nullable
                                         (cli/invoke {:prompt "test"}))]
          (is (= 42 (:exit-code @result-promise))))))

    (testing "with custom events"
      (testing "returns configured events"
        (let [events [{:type "init" :session_id "s1"}
                      {:type "result" :data "ok"}]
              nullable (cli/make-nullable {:events events})
              {:keys [result-promise]} (cli/with-nullable-claude-cli nullable
                                         (cli/invoke {:prompt "test"}))]
          (is (= events (:events @result-promise))))))

    (testing "with custom session-id"
      (testing "returns configured session-id"
        (let [nullable (cli/make-nullable {:session-id "custom-sess"})
              {:keys [result-promise]} (cli/with-nullable-claude-cli nullable
                                         (cli/invoke {:prompt "test"}))]
          (is (= "custom-sess" (:session-id @result-promise))))))

    (testing "with error config"
      (testing "returns error result with nil exit-code"
        (let [nullable (cli/make-nullable {:error :timeout})
              {:keys [result-promise]} (cli/with-nullable-claude-cli nullable
                                         (cli/invoke {:prompt "test"}))
              result @result-promise]
          (is (nil? (:exit-code result)))
          (is (= :timeout (:error result))))))))

(deftest invocations-test
  (testing "invocations"
    (testing "with no invocations"
      (testing "returns empty vector"
        (let [nullable (cli/make-nullable)]
          (is (= [] (cli/invocations nullable))))))

    (testing "after single invocation"
      (testing "records opts and timestamp"
        (let [nullable (cli/make-nullable)
              opts {:prompt "hello" :model "sonnet"}]
          (cli/with-nullable-claude-cli nullable
            (cli/invoke opts))
          (let [invs (cli/invocations nullable)]
            (is (= 1 (count invs)))
            (is (= opts (:opts (first invs))))
            (is (instance? java.time.Instant (:timestamp (first invs))))))))

    (testing "after multiple invocations"
      (testing "records all invocations in order"
        (let [nullable (cli/make-nullable)]
          (cli/with-nullable-claude-cli nullable
            (cli/invoke {:prompt "first"})
            (cli/invoke {:prompt "second"})
            (cli/invoke {:prompt "third"}))
          (let [invs (cli/invocations nullable)]
            (is (= 3 (count invs)))
            (is (= ["first" "second" "third"]
                   (mapv #(get-in % [:opts :prompt]) invs)))))))))

(deftest nullable-isolation-test
  (testing "nullable isolation"
    (testing "different nullables track separately"
      (let [nullable1 (cli/make-nullable)
            nullable2 (cli/make-nullable)]
        (cli/with-nullable-claude-cli nullable1
          (cli/invoke {:prompt "for-1"}))
        (cli/with-nullable-claude-cli nullable2
          (cli/invoke {:prompt "for-2a"})
          (cli/invoke {:prompt "for-2b"}))
        (is (= 1 (count (cli/invocations nullable1))))
        (is (= 2 (count (cli/invocations nullable2))))
        (is (= "for-1" (get-in (first (cli/invocations nullable1)) [:opts :prompt])))
        (is (= "for-2a" (get-in (first (cli/invocations nullable2)) [:opts :prompt])))))))
