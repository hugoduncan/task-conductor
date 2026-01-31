(ns task-conductor.dev-env.interface-test
  ;; Verify the interface namespace wraps the protocol correctly and
  ;; provides helper functions for common operations.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.interface :as interface]
   [task-conductor.dev-env.protocol :as protocol]))

;;; Protocol wrapper tests

(deftest start-session-test
  (testing "start-session"
    (testing "delegates to protocol/start-session"
      (let [dev-env (protocol/make-noop-dev-env)
            result (interface/start-session dev-env "sess-123" {:dir "/tmp"})]
        (is (= {:session-id "sess-123" :handle :noop} result))
        (let [calls @(:calls dev-env)]
          (is (= :start-session (:op (first calls)))))))))

(deftest register-hook-test
  (testing "register-hook"
    (testing "delegates to protocol/register-hook for :on-close"
      (let [dev-env (protocol/make-noop-dev-env)
            callback (fn [_] :test)
            result (interface/register-hook dev-env :on-close callback)]
        (is (uuid? result))
        (let [calls @(:calls dev-env)]
          (is (= :register-hook (:op (first calls)))))))

    (testing "delegates to protocol/register-hook for :on-idle"
      (let [dev-env (protocol/make-noop-dev-env)
            callback (fn [_] :test)
            result (interface/register-hook dev-env :on-idle callback)]
        (is (uuid? result))
        (let [calls @(:calls dev-env)]
          (is (= :register-hook (:op (first calls)))))))

    (testing "throws on unsupported hook-type"
      (let [dev-env (protocol/make-noop-dev-env)
            callback (fn [_] :test)]
        (is (thrown-with-msg?
             clojure.lang.ExceptionInfo
             #"Unsupported hook-type: :on-unknown"
             (interface/register-hook dev-env :on-unknown callback)))
        (try
          (interface/register-hook dev-env :on-unknown callback)
          (catch clojure.lang.ExceptionInfo e
            (is (= :on-unknown (:hook-type (ex-data e))))
            (is (= #{:on-close :on-idle} (:valid-hook-types (ex-data e))))))))))

(deftest query-transcript-test
  (testing "query-transcript"
    (testing "delegates to protocol/query-transcript"
      (let [dev-env (protocol/make-noop-dev-env)
            result (interface/query-transcript dev-env "sess-456")]
        (is (nil? result))
        (let [calls @(:calls dev-env)]
          (is (= :query-transcript (:op (first calls)))))))))

(deftest query-events-test
  (testing "query-events"
    (testing "delegates to protocol/query-events"
      (let [dev-env (protocol/make-noop-dev-env)
            result (interface/query-events dev-env "sess-789")]
        (is (nil? result))
        (let [calls @(:calls dev-env)]
          (is (= :query-events (:op (first calls)))))))))

(deftest close-session-test
  (testing "close-session"
    (testing "delegates to protocol/close-session"
      (let [dev-env (protocol/make-noop-dev-env)
            result (interface/close-session dev-env "sess-xyz")]
        (is (false? result))
        (let [calls @(:calls dev-env)]
          (is (= :close-session (:op (first calls)))))))))

;;; Helper function tests

(deftest register-hooks-test
  (testing "register-hooks"
    (testing "registers multiple hooks at once"
      (let [dev-env (protocol/make-noop-dev-env)
            on-close (fn [_] :closed)
            on-idle (fn [_] :idle)
            result (interface/register-hooks dev-env {:on-close on-close
                                                      :on-idle on-idle})]
        (is (= #{:on-close :on-idle} (set (keys result))))
        (is (uuid? (:on-close result)))
        (is (uuid? (:on-idle result)))
        (is (not= (:on-close result) (:on-idle result)))
        (let [calls @(:calls dev-env)]
          (is (= 2 (count calls)))
          (is (= #{:on-close :on-idle}
                 (set (map :hook-type calls)))))))

    (testing "returns empty map for empty hook-map"
      (let [dev-env (protocol/make-noop-dev-env)
            result (interface/register-hooks dev-env {})]
        (is (= {} result))
        (is (empty? @(:calls dev-env)))))))

(deftest with-session-test
  (testing "with-session"
    (testing "starts session, executes body, and closes"
      (let [dev-env (protocol/make-noop-dev-env)
            body-result (interface/with-session [session dev-env "s1" {:dir "/x"}]
                          (is (= {:session-id "s1" :handle :noop} session))
                          :body-done)]
        (is (= :body-done body-result))
        (let [calls @(:calls dev-env)
              ops (mapv :op calls)]
          (is (= [:start-session :close-session] ops))
          (is (= "s1" (:session-id (first calls))))
          (is (= {:dir "/x"} (:opts (first calls))))
          (is (= "s1" (:session-id (second calls)))))))

    (testing "closes session even when body throws"
      (let [dev-env (protocol/make-noop-dev-env)]
        (is (thrown-with-msg? Exception #"test error"
                              (interface/with-session [_session dev-env "s2" nil]
                                (throw (Exception. "test error")))))
        (let [calls @(:calls dev-env)
              ops (mapv :op calls)]
          (is (= [:start-session :close-session] ops))
          (is (= "s2" (:session-id (second calls)))))))

    (testing "binds session handle for use in body"
      (let [dev-env (protocol/make-noop-dev-env)
            captured-session (atom nil)]
        (interface/with-session [session dev-env "s3" nil]
          (reset! captured-session session))
        (is (= {:session-id "s3" :handle :noop} @captured-session))))))
