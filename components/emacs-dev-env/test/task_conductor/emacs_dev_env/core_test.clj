(ns task-conductor.emacs-dev-env.core-test
  ;; Verify EmacsDevEnv implements DevEnv protocol correctly, with
  ;; command queueing and response handling via core.async channels.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.protocol :as protocol]
   [task-conductor.emacs-dev-env.core :as core]))

(deftest make-emacs-dev-env-test
  (testing "make-emacs-dev-env"
    (testing "creates an unconnected dev-env"
      (let [dev-env (core/make-emacs-dev-env)]
        (is (not (core/connected? dev-env)))
        (core/shutdown dev-env)))

    (testing "initializes with empty hooks and sessions"
      (let [dev-env (core/make-emacs-dev-env)
            state @(:state dev-env)]
        (is (= {} (:hooks state)))
        (is (= {} (:sessions state)))
        (core/shutdown dev-env)))))

(deftest register-emacs-dev-env-test
  (testing "register-emacs-dev-env"
    (testing "marks dev-env as connected"
      (let [dev-env (core/make-emacs-dev-env)]
        (is (true? (core/register-emacs-dev-env dev-env)))
        (is (core/connected? dev-env))
        (core/shutdown dev-env)))))

(deftest await-command-test
  (testing "await-command"
    (testing "returns command without response-promise"
      (let [dev-env (core/make-emacs-dev-env)
            ;; Simulate a protocol call in a separate thread
            result-future (future
                            (protocol/start-session dev-env "s1" {:dir "/tmp"}))
            ;; Give it time to put command on channel
            _ (Thread/sleep 50)
            cmd (core/await-command dev-env)]
        (is (uuid? (:command-id cmd)))
        (is (= :start-session (:command cmd)))
        (is (= {:session-id "s1" :opts {:dir "/tmp"}} (:params cmd)))
        (is (not (contains? cmd :response-promise)))
        ;; Send response to unblock the future
        (core/send-response dev-env (:command-id cmd) {:handle :test})
        (is (= {:handle :test} @result-future))
        (core/shutdown dev-env)))

    (testing "returns nil when channel closed"
      (let [dev-env (core/make-emacs-dev-env)]
        (core/shutdown dev-env)
        (is (nil? (core/await-command dev-env)))))))

(deftest send-response-test
  (testing "send-response"
    (testing "delivers response to waiting caller"
      (let [dev-env (core/make-emacs-dev-env)
            result-future (future
                            (protocol/query-transcript dev-env "s1"))
            _ (Thread/sleep 50)
            cmd (core/await-command dev-env)]
        (is (true? (core/send-response dev-env (:command-id cmd) "transcript")))
        (is (= "transcript" @result-future))
        (core/shutdown dev-env)))

    (testing "returns false for unknown command-id"
      (let [dev-env (core/make-emacs-dev-env)]
        (is (false? (core/send-response dev-env (java.util.UUID/randomUUID) "x")))
        (core/shutdown dev-env)))))

(deftest register-hook-test
  (testing "register-hook"
    (testing "stores hook and returns uuid"
      (let [dev-env (core/make-emacs-dev-env)
            callback (fn [_] :called)
            hook-id (protocol/register-hook dev-env :on-idle callback)]
        (is (uuid? hook-id))
        (let [hook (get-in @(:state dev-env) [:hooks hook-id])]
          (is (= :on-idle (:type hook)))
          (is (= callback (:callback hook))))
        (core/shutdown dev-env)))))

(deftest send-hook-event-test
  (testing "send-hook-event"
    (testing "invokes matching hooks with context"
      (let [dev-env (core/make-emacs-dev-env)
            received (atom nil)
            callback (fn [ctx] (reset! received ctx))]
        (protocol/register-hook dev-env :on-idle callback)
        (core/send-hook-event dev-env :on-idle "sess-1" :idle)
        (is (= "sess-1" (:session-id @received)))
        (is (= :idle (:reason @received)))
        (is (inst? (:timestamp @received)))
        (core/shutdown dev-env)))

    (testing "does not invoke non-matching hooks"
      (let [dev-env (core/make-emacs-dev-env)
            idle-called (atom false)
            close-called (atom false)]
        (protocol/register-hook dev-env :on-idle (fn [_] (reset! idle-called true)))
        (protocol/register-hook dev-env :on-close (fn [_] (reset! close-called true)))
        (core/send-hook-event dev-env :on-close "s1" :user-exit)
        (is (false? @idle-called))
        (is (true? @close-called))
        (core/shutdown dev-env)))))

(deftest close-session-test
  (testing "close-session"
    (testing "removes session from state when successful"
      (let [dev-env (core/make-emacs-dev-env)
            ;; First start a session
            start-future (future
                           (protocol/start-session dev-env "s1" {}))
            _ (Thread/sleep 50)
            start-cmd (core/await-command dev-env)]
        (core/send-response dev-env (:command-id start-cmd) {:handle :test})
        @start-future
        ;; Session should be tracked
        (is (contains? (:sessions @(:state dev-env)) "s1"))
        ;; Now close it
        (let [close-future (future
                             (protocol/close-session dev-env "s1"))
              _ (Thread/sleep 50)
              close-cmd (core/await-command dev-env)]
          (core/send-response dev-env (:command-id close-cmd) true)
          (is (true? @close-future))
          ;; Session should be removed
          (is (not (contains? (:sessions @(:state dev-env)) "s1"))))
        (core/shutdown dev-env)))))

(deftest command-queue-ordering-test
  (testing "command queue"
    (testing "maintains FIFO order"
      (let [dev-env (core/make-emacs-dev-env)
            ;; Queue multiple commands
            f1 (future (protocol/query-transcript dev-env "s1"))
            _ (Thread/sleep 20)
            f2 (future (protocol/query-events dev-env "s2"))
            _ (Thread/sleep 20)
            ;; Read them in order
            cmd1 (core/await-command dev-env)
            cmd2 (core/await-command dev-env)]
        (is (= :query-transcript (:command cmd1)))
        (is (= "s1" (get-in cmd1 [:params :session-id])))
        (is (= :query-events (:command cmd2)))
        (is (= "s2" (get-in cmd2 [:params :session-id])))
        ;; Respond to unblock
        (core/send-response dev-env (:command-id cmd1) nil)
        (core/send-response dev-env (:command-id cmd2) nil)
        @f1
        @f2
        (core/shutdown dev-env)))))
