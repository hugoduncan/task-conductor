(ns task-conductor.dev-env.interface-test
  ;; Tests that the DevEnv protocol can be implemented and used correctly.
  ;; Verifies the contract: open-cli-session accepts opts/callback, returns
  ;; {:status :requested}; close-session accepts session-id, returns
  ;; {:status :requested}; callbacks receive expected result structure.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.interface :as dev-env]))

;;; Test implementation

(defrecord MockDevEnv [sessions]
  dev-env/DevEnv

  (open-cli-session [_this opts callback]
    (swap! sessions assoc (:session-id opts) {:opts opts :callback callback})
    {:status :requested})

  (close-session [_this session-id]
    (swap! sessions dissoc session-id)
    {:status :requested}))

(defn make-mock-dev-env
  "Create a mock DevEnv for testing."
  []
  (->MockDevEnv (atom {})))

(defn complete-session
  "Simulate session completion by invoking the stored callback."
  [mock-env session-id result]
  (when-let [session (get @(:sessions mock-env) session-id)]
    ((:callback session) result)))

;;; Tests

(deftest dev-env-protocol-test
  (testing "DevEnv protocol"
    (testing "can be implemented"
      (let [env (make-mock-dev-env)]
        (is (satisfies? dev-env/DevEnv env))))

    (testing "open-cli-session"
      (testing "returns {:status :requested}"
        (let [env (make-mock-dev-env)
              result (dev-env/open-cli-session
                      env
                      {:session-id "test-123"
                       :prompt "hello"
                       :working-dir "/tmp"}
                      identity)]
          (is (= {:status :requested} result))))

      (testing "stores session with opts and callback"
        (let [env (make-mock-dev-env)
              cb-called (atom nil)
              callback (fn [r] (reset! cb-called r))
              opts {:session-id "sess-456"
                    :prompt "test prompt"
                    :working-dir "/home"}]
          (dev-env/open-cli-session env opts callback)
          (let [stored (get @(:sessions env) "sess-456")]
            (is (= opts (:opts stored)))
            (is (= callback (:callback stored)))))))

    (testing "close-session"
      (testing "returns {:status :requested}"
        (let [env (make-mock-dev-env)]
          (dev-env/open-cli-session
           env
           {:session-id "to-close"}
           identity)
          (is (= {:status :requested}
                 (dev-env/close-session env "to-close")))))

      (testing "removes session from tracking"
        (let [env (make-mock-dev-env)]
          (dev-env/open-cli-session
           env
           {:session-id "remove-me"}
           identity)
          (is (contains? @(:sessions env) "remove-me"))
          (dev-env/close-session env "remove-me")
          (is (not (contains? @(:sessions env) "remove-me"))))))

    (testing "callback invocation"
      (testing "receives result with expected keys"
        (let [env (make-mock-dev-env)
              received (promise)
              callback (fn [r] (deliver received r))]
          (dev-env/open-cli-session
           env
           {:session-id "cb-test"}
           callback)
          (complete-session env "cb-test"
                            {:session-id "cb-test"
                             :status :completed
                             :hook-status {:state "done"}
                             :exit-code 0})
          (let [result (deref received 500 :timeout)]
            (is (not= :timeout result))
            (is (= "cb-test" (:session-id result)))
            (is (= :completed (:status result)))
            (is (= {:state "done"} (:hook-status result)))
            (is (= 0 (:exit-code result)))))))))
