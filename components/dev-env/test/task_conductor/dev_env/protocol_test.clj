(ns task-conductor.dev-env.protocol-test
  ;; Verify NoOpDevEnv correctly implements the DevEnv protocol,
  ;; tracking all calls and returning appropriate default values.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.protocol :as protocol]))

(deftest start-session-test
  (testing "start-session"
    (testing "records the call with session-id and opts"
      (let [dev-env (protocol/make-noop-dev-env)
            result (protocol/start-session dev-env "sess-123" {:dir "/tmp"})]
        (is (= {:session-id "sess-123" :handle :noop} result))
        (let [calls @(:calls dev-env)
              call (first calls)]
          (is (= 1 (count calls)))
          (is (= :start-session (:op call)))
          (is (= "sess-123" (:session-id call)))
          (is (= {:dir "/tmp"} (:opts call)))
          (is (inst? (:timestamp call))))))

    (testing "handles nil opts"
      (let [dev-env (protocol/make-noop-dev-env)
            result (protocol/start-session dev-env "sess-456" nil)]
        (is (= {:session-id "sess-456" :handle :noop} result))))))

(deftest register-hook-test
  (testing "register-hook"
    (testing "records the hook registration"
      (let [dev-env (protocol/make-noop-dev-env)
            callback (fn [_ctx] :called)
            hook-id (protocol/register-hook dev-env :on-close callback)]
        (is (uuid? hook-id))
        (let [calls @(:calls dev-env)
              call (first calls)]
          (is (= 1 (count calls)))
          (is (= :register-hook (:op call)))
          (is (= :on-close (:hook-type call)))
          (is (= hook-id (:hook-id call))))))

    (testing "stores hook in hooks atom"
      (let [dev-env (protocol/make-noop-dev-env)
            callback (fn [_ctx] :called)
            hook-id (protocol/register-hook dev-env :on-idle callback)
            hooks @(:hooks dev-env)
            hook (get hooks hook-id)]
        (is (= :on-idle (:type hook)))
        (is (= callback (:callback hook)))))

    (testing "generates unique hook-ids"
      (let [dev-env (protocol/make-noop-dev-env)
            id1 (protocol/register-hook dev-env :on-close (fn [_] nil))
            id2 (protocol/register-hook dev-env :on-idle (fn [_] nil))]
        (is (not= id1 id2))))))

(deftest query-transcript-test
  (testing "query-transcript"
    (testing "records the call and returns nil"
      (let [dev-env (protocol/make-noop-dev-env)
            result (protocol/query-transcript dev-env "sess-789")]
        (is (nil? result))
        (let [calls @(:calls dev-env)
              call (first calls)]
          (is (= 1 (count calls)))
          (is (= :query-transcript (:op call)))
          (is (= "sess-789" (:session-id call))))))))

(deftest query-events-test
  (testing "query-events"
    (testing "records the call and returns nil"
      (let [dev-env (protocol/make-noop-dev-env)
            result (protocol/query-events dev-env "sess-abc")]
        (is (nil? result))
        (let [calls @(:calls dev-env)
              call (first calls)]
          (is (= 1 (count calls)))
          (is (= :query-events (:op call)))
          (is (= "sess-abc" (:session-id call))))))))

(deftest close-session-test
  (testing "close-session"
    (testing "records the call and returns false"
      (let [dev-env (protocol/make-noop-dev-env)
            result (protocol/close-session dev-env "sess-xyz")]
        (is (false? result))
        (let [calls @(:calls dev-env)
              call (first calls)]
          (is (= 1 (count calls)))
          (is (= :close-session (:op call)))
          (is (= "sess-xyz" (:session-id call))))))))

(deftest call-tracking-test
  (testing "call tracking"
    (testing "records multiple calls in order"
      (let [dev-env (protocol/make-noop-dev-env)]
        (protocol/start-session dev-env "s1" nil)
        (protocol/query-transcript dev-env "s1")
        (protocol/query-events dev-env "s1")
        (protocol/close-session dev-env "s1")
        (let [calls @(:calls dev-env)
              ops (mapv :op calls)]
          (is (= 4 (count calls)))
          (is (= [:start-session :query-transcript :query-events :close-session]
                 ops)))))))
