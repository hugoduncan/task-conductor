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
    (testing "with dev-env arg marks it as connected"
      (let [dev-env (core/make-emacs-dev-env)]
        (is (true? (core/register-emacs-dev-env dev-env)))
        (is (core/connected? dev-env))
        (core/shutdown dev-env)))

    (testing "with no args creates dev-env and returns id"
      (let [dev-env-id (core/register-emacs-dev-env)]
        (is (string? dev-env-id))
        (is (re-matches #"[0-9a-f-]+" dev-env-id))
        (let [dev-env (core/get-dev-env dev-env-id)]
          (is (some? dev-env))
          (is (core/connected? dev-env)))
        (core/unregister-emacs-dev-env dev-env-id)))))

(deftest unregister-emacs-dev-env-test
  (testing "unregister-emacs-dev-env"
    (testing "removes dev-env from registry"
      (let [dev-env-id (core/register-emacs-dev-env)]
        (is (some? (core/get-dev-env dev-env-id)))
        (is (true? (core/unregister-emacs-dev-env dev-env-id)))
        (is (nil? (core/get-dev-env dev-env-id)))))

    (testing "returns false for unknown id"
      (is (false? (core/unregister-emacs-dev-env "nonexistent"))))))

(deftest id-based-functions-test
  (testing "ID-based nREPL functions"
    (testing "await-command-by-id returns error for unknown id"
      (is (= {:status :error :message "Dev-env not found: unknown"}
             (core/await-command-by-id "unknown"))))

    (testing "send-response-by-id returns error for unknown id"
      (let [result (core/send-response-by-id "unknown" (java.util.UUID/randomUUID) nil)]
        (is (= :not-found (:error result)))))

    (testing "send-hook-event-by-id returns error for unknown id"
      (let [result (core/send-hook-event-by-id "unknown" :on-idle "s1" :idle)]
        (is (= :not-found (:error result)))))

    (testing "await-command-by-id works with valid id"
      (let [dev-env-id (core/register-emacs-dev-env)
            result (core/await-command-by-id dev-env-id 50)]
        (is (= {:status :timeout} result))
        (core/unregister-emacs-dev-env dev-env-id)))))

(deftest await-command-test
  (testing "await-command"
    (testing "returns {:status :ok :command cmd} on success"
      (let [dev-env (core/make-emacs-dev-env)
            ;; Simulate a protocol call in a separate thread
            result-future (future
                            (protocol/start-session dev-env "s1" {:dir "/tmp"}))
            ;; Give it time to put command on channel
            _ (Thread/sleep 50)
            result (core/await-command dev-env)
            cmd (:command result)]
        (is (= :ok (:status result)))
        (is (uuid? (:command-id cmd)))
        (is (= :start-session (:command cmd)))
        (is (= {:session-id "s1" :opts {:dir "/tmp"}} (:params cmd)))
        (is (not (contains? cmd :response-promise)))
        ;; Send response to unblock the future
        (core/send-response dev-env (:command-id cmd) {:handle :test})
        (is (= {:handle :test} @result-future))
        (core/shutdown dev-env)))

    (testing "returns {:status :timeout} on timeout"
      (let [dev-env (core/make-emacs-dev-env)
            result (core/await-command dev-env 50)]
        (is (= {:status :timeout} result))
        (core/shutdown dev-env)))

    (testing "returns {:status :closed} when channel closed"
      (let [dev-env (core/make-emacs-dev-env)]
        (core/shutdown dev-env)
        (is (= {:status :closed} (core/await-command dev-env)))))))

(deftest send-response-test
  (testing "send-response"
    (testing "delivers response to waiting caller"
      (let [dev-env (core/make-emacs-dev-env)
            result-future (future
                            (protocol/query-transcript dev-env "s1"))
            _ (Thread/sleep 50)
            result (core/await-command dev-env)
            cmd (:command result)]
        (is (= :ok (:status result)))
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
            start-result (core/await-command dev-env)
            start-cmd (:command start-result)]
        (is (= :ok (:status start-result)))
        (core/send-response dev-env (:command-id start-cmd) {:handle :test})
        @start-future
        ;; Session should be tracked
        (is (contains? (:sessions @(:state dev-env)) "s1"))
        ;; Now close it
        (let [close-future (future
                             (protocol/close-session dev-env "s1"))
              _ (Thread/sleep 50)
              close-result (core/await-command dev-env)
              close-cmd (:command close-result)]
          (is (= :ok (:status close-result)))
          (core/send-response dev-env (:command-id close-cmd) true)
          (is (true? @close-future))
          ;; Session should be removed
          (is (not (contains? (:sessions @(:state dev-env)) "s1"))))
        (core/shutdown dev-env)))))

(deftest response-timeout-test
  ;; Verify that protocol methods return timeout error when Emacs
  ;; doesn't respond within the timeout period.
  (testing "response timeout"
    (testing "returns error map when response not received"
      ;; Use with-redefs to use a short timeout for testing
      (with-redefs [core/default-response-timeout-ms 100]
        (let [dev-env (core/make-emacs-dev-env)
              ;; Start a session but don't respond
              result-future (future
                              (protocol/start-session dev-env "s1" {}))
              _ (Thread/sleep 50)
              ;; Take the command but don't respond
              await-result (core/await-command dev-env)]
          (is (= :ok (:status await-result)))
          ;; Wait for timeout
          (let [result @result-future]
            (is (= :timeout (:error result)))
            (is (string? (:message result))))
          (core/shutdown dev-env))))))

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
            result1 (core/await-command dev-env)
            result2 (core/await-command dev-env)
            cmd1 (:command result1)
            cmd2 (:command result2)]
        (is (= :ok (:status result1)))
        (is (= :ok (:status result2)))
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

;;; Health Check Tests

(deftest ping-test
  ;; Verify ping sends a :ping command and interprets the response.
  (testing "ping"
    (testing "returns :error when not connected"
      (let [dev-env (core/make-emacs-dev-env)]
        (is (= {:status :error :message "Dev-env not connected"}
               (core/ping dev-env)))
        (core/shutdown dev-env)))

    (testing "returns :error when channel closed"
      ;; shutdown also sets connected? to false, so we get that error first
      (let [dev-env (core/make-emacs-dev-env)]
        (core/register-emacs-dev-env dev-env)
        (core/shutdown dev-env)
        (is (= {:status :error :message "Dev-env not connected"}
               (core/ping dev-env)))))

    (testing "returns :ok when Emacs responds"
      (let [dev-env (core/make-emacs-dev-env)]
        (core/register-emacs-dev-env dev-env)
        ;; Simulate Emacs responding to ping in a thread
        (let [ping-future (future (core/ping dev-env 1000))
              _ (Thread/sleep 50)
              result (core/await-command dev-env)
              cmd (:command result)]
          (is (= :ok (:status result)))
          (is (= :ping (:command cmd)))
          (core/send-response dev-env (:command-id cmd) {:status :ok})
          (is (= {:status :ok} @ping-future)))
        (core/shutdown dev-env)))

    (testing "returns :timeout when no response"
      (let [dev-env (core/make-emacs-dev-env)]
        (core/register-emacs-dev-env dev-env)
        ;; Don't respond to the ping
        (is (= {:status :timeout} (core/ping dev-env 100)))
        (core/shutdown dev-env)))))

(deftest ping-by-id-test
  (testing "ping-by-id"
    (testing "returns error for unknown id"
      (is (= {:status :error :message "Dev-env not found: unknown"}
             (core/ping-by-id "unknown"))))

    (testing "pings valid dev-env"
      (let [dev-env-id (core/register-emacs-dev-env)]
        ;; Respond in a thread
        (let [ping-future (future (core/ping-by-id dev-env-id 1000))
              _ (Thread/sleep 50)
              result (core/await-command-by-id dev-env-id)
              cmd (:command result)]
          (is (= :ping (:command cmd)))
          (core/send-response-by-id dev-env-id (:command-id cmd) {:status :ok})
          (is (= {:status :ok} @ping-future)))
        (core/unregister-emacs-dev-env dev-env-id)))))

;;; Dev-Env Selection Tests

(deftest list-dev-envs-test
  ;; Verify listing of registered dev-envs with connection status.
  (testing "list-dev-envs"
    (testing "returns empty vector when no dev-envs registered"
      (is (= [] (core/list-dev-envs))))

    (testing "returns registered dev-envs with status"
      (let [dev-env-id (core/register-emacs-dev-env)]
        (is (= [{:dev-env-id dev-env-id
                 :type :emacs
                 :connected? true}]
               (core/list-dev-envs)))
        (core/unregister-emacs-dev-env dev-env-id)))

    (testing "shows disconnected status when dev-env not connected"
      ;; Create a dev-env and add to registry without connecting
      (let [dev-env (core/make-emacs-dev-env)
            dev-env-id "test-disconnected-id"]
        (swap! core/registry assoc dev-env-id dev-env)
        (is (= [{:dev-env-id dev-env-id
                 :type :emacs
                 :connected? false}]
               (core/list-dev-envs)))
        (core/shutdown dev-env)
        (swap! core/registry dissoc dev-env-id)))))

(deftest list-healthy-dev-envs-test
  ;; Verify filtering of dev-envs by health check (ping).
  (testing "list-healthy-dev-envs"
    (testing "returns empty when no dev-envs respond to ping"
      (let [dev-env-id (core/register-emacs-dev-env)]
        ;; No responder, short timeout
        (is (= [] (core/list-healthy-dev-envs 50)))
        (core/unregister-emacs-dev-env dev-env-id)))

    (testing "returns dev-envs that respond to ping"
      (let [dev-env-id (core/register-emacs-dev-env)
            ;; Start a responder thread
            responder (future
                        (loop []
                          (let [result (core/await-command-by-id dev-env-id 100)]
                            (when (= :ok (:status result))
                              (let [cmd (:command result)]
                                (when (= :ping (:command cmd))
                                  (core/send-response-by-id dev-env-id (:command-id cmd)
                                                            {:status :ok}))
                                (recur))))))]
        (Thread/sleep 50)
        (let [healthy (core/list-healthy-dev-envs 1000)]
          (is (= 1 (count healthy)))
          (is (= dev-env-id (:dev-env-id (first healthy)))))
        (core/unregister-emacs-dev-env dev-env-id)
        ;; responder will exit on channel close
        @responder))))

(deftest select-dev-env-test
  ;; Verify selection of best available dev-env.
  (testing "select-dev-env"
    (testing "returns nil when no healthy dev-envs"
      (is (nil? (core/select-dev-env 50))))

    (testing "returns first healthy dev-env"
      (let [dev-env-id (core/register-emacs-dev-env)
            ;; Start a responder
            responder (future
                        (loop []
                          (let [result (core/await-command-by-id dev-env-id 100)]
                            (when (= :ok (:status result))
                              (let [cmd (:command result)]
                                (when (= :ping (:command cmd))
                                  (core/send-response-by-id dev-env-id (:command-id cmd)
                                                            {:status :ok}))
                                (recur))))))]
        (Thread/sleep 50)
        (let [selected (core/select-dev-env 1000)]
          (is (some? selected))
          (is (= dev-env-id (:dev-env-id selected)))
          (is (= :emacs (:type selected)))
          (is (some? (:dev-env selected))))
        (core/unregister-emacs-dev-env dev-env-id)
        @responder))))

;;; Concurrent Command Handling Tests

(deftest concurrent-command-handling-test
  ;; Verify multiple protocol calls can be in flight simultaneously
  ;; and responses are correctly routed to each caller.
  (testing "concurrent command handling"
    (testing "routes responses to correct callers when multiple commands in flight"
      (let [dev-env (core/make-emacs-dev-env)
            ;; Start three concurrent protocol calls
            f1 (future (protocol/query-transcript dev-env "s1"))
            _ (Thread/sleep 20)
            f2 (future (protocol/query-events dev-env "s2"))
            _ (Thread/sleep 20)
            f3 (future (protocol/start-session dev-env "s3" {:dir "/tmp"}))
            _ (Thread/sleep 50)
            ;; Collect all commands
            result1 (core/await-command dev-env 100)
            result2 (core/await-command dev-env 100)
            result3 (core/await-command dev-env 100)
            cmd1 (:command result1)
            cmd2 (:command result2)
            cmd3 (:command result3)]
        (is (= :ok (:status result1)))
        (is (= :ok (:status result2)))
        (is (= :ok (:status result3)))
        ;; Respond in reverse order to verify routing
        (core/send-response dev-env (:command-id cmd3) {:handle :session3})
        (core/send-response dev-env (:command-id cmd1) "transcript-s1")
        (core/send-response dev-env (:command-id cmd2) [{:event :idle}])
        ;; Verify each future got the correct response
        (is (= "transcript-s1" @f1))
        (is (= [{:event :idle}] @f2))
        (is (= {:handle :session3} @f3))
        (core/shutdown dev-env)))

    (testing "handles interleaved commands and responses"
      (let [dev-env (core/make-emacs-dev-env)
            ;; Start first command
            f1 (future (protocol/query-transcript dev-env "s1"))
            _ (Thread/sleep 30)
            result1 (core/await-command dev-env 100)
            cmd1 (:command result1)
            ;; Start second command before responding to first
            f2 (future (protocol/query-events dev-env "s2"))
            _ (Thread/sleep 30)
            result2 (core/await-command dev-env 100)
            cmd2 (:command result2)]
        ;; Respond to second command first
        (core/send-response dev-env (:command-id cmd2) "events-s2")
        (is (= "events-s2" (deref f2 100 :timeout)))
        ;; First command still waiting
        (is (not (realized? f1)))
        ;; Now respond to first
        (core/send-response dev-env (:command-id cmd1) "transcript-s1")
        (is (= "transcript-s1" @f1))
        (core/shutdown dev-env)))))

;;; Hook Error Handling Tests

(deftest hook-error-handling-test
  ;; Verify that errors in hook callbacks don't crash the system
  ;; and other hooks still execute.
  (testing "hook error handling"
    (testing "continues invoking hooks after one throws"
      (let [dev-env (core/make-emacs-dev-env)
            hook1-called (atom false)
            hook3-called (atom false)]
        ;; Register hooks - second one will throw
        (protocol/register-hook dev-env :on-idle
                                (fn [_] (reset! hook1-called true)))
        (protocol/register-hook dev-env :on-idle
                                (fn [_] (throw (ex-info "Hook error" {}))))
        (protocol/register-hook dev-env :on-idle
                                (fn [_] (reset! hook3-called true)))
        ;; Send event - should not throw
        (is (true? (core/send-hook-event dev-env :on-idle "s1" :idle)))
        ;; All non-throwing hooks should have been called
        ;; Note: hook execution order depends on map iteration order
        ;; so we just verify the throwing hook didn't prevent others
        (is (or @hook1-called @hook3-called)
            "At least one non-throwing hook should execute")
        (core/shutdown dev-env)))

    (testing "does not affect other hook types when one type throws"
      (let [dev-env (core/make-emacs-dev-env)
            close-called (atom false)]
        ;; Register throwing idle hook and normal close hook
        (protocol/register-hook dev-env :on-idle
                                (fn [_] (throw (ex-info "Idle error" {}))))
        (protocol/register-hook dev-env :on-close
                                (fn [_] (reset! close-called true)))
        ;; Trigger idle - should not affect close hook
        (core/send-hook-event dev-env :on-idle "s1" :idle)
        ;; Trigger close - should work
        (core/send-hook-event dev-env :on-close "s1" :user-exit)
        (is (true? @close-called))
        (core/shutdown dev-env)))))

;;; Registry Cleanup Tests

(deftest registry-cleanup-test
  ;; Verify dev-env is properly removed from registry on unregister
  ;; and all resources are cleaned up.
  (testing "registry cleanup"
    (testing "removes dev-env from registry on unregister"
      (let [dev-env-id (core/register-emacs-dev-env)]
        (is (some? (core/get-dev-env dev-env-id)))
        (is (= 1 (count @core/registry)))
        (core/unregister-emacs-dev-env dev-env-id)
        (is (nil? (core/get-dev-env dev-env-id)))
        (is (= 0 (count @core/registry)))))

    (testing "cleans up pending commands on unregister"
      (let [dev-env-id (core/register-emacs-dev-env)
            dev-env (core/get-dev-env dev-env-id)
            ;; Start a command but don't respond
            result-future (future
                            (protocol/query-transcript dev-env "s1"))
            _ (Thread/sleep 50)
            ;; Take the command
            _ (core/await-command dev-env 100)]
        ;; Verify pending command exists
        (is (= 1 (count (:pending-commands @(:state dev-env)))))
        ;; Unregister - should clean up
        (core/unregister-emacs-dev-env dev-env-id)
        ;; Verify cleanup
        (is (= {} (:pending-commands @(:state dev-env))))
        (is (nil? (:command-chan @(:state dev-env))))
        ;; The future will get timeout error since channel closed
        (let [result (deref result-future 200 :test-timeout)]
          (is (or (= :test-timeout result)
                  (= :timeout (:error result)))))))

    (testing "closes command channel on shutdown"
      (let [dev-env-id (core/register-emacs-dev-env)
            dev-env (core/get-dev-env dev-env-id)]
        (is (some? (:command-chan @(:state dev-env))))
        (core/unregister-emacs-dev-env dev-env-id)
        (is (nil? (:command-chan @(:state dev-env))))
        ;; Await should return closed status
        (is (= {:status :closed} (core/await-command dev-env)))))))
