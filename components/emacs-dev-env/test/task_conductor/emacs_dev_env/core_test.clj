(ns task-conductor.emacs-dev-env.core-test
  ;; Verify EmacsDevEnv implements DevEnv protocol correctly, with
  ;; command queueing and response handling via core.async channels.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.protocol :as protocol]
   [task-conductor.dev-env.registry :as generic-registry]
   [task-conductor.emacs-dev-env.core :as core]
   [task-conductor.project.registry :as project-registry]
   [task-conductor.project.resolvers :as project-resolvers]
   [task-conductor.statechart-engine.resolvers :as resolvers]))

(defmacro with-register-hook-responder
  "Spawn a thread that responds to :register-hook commands with success.
  Body executes while responder is active; thread stops on exit."
  [dev-env & body]
  `(let [dev-env# ~dev-env
         running# (atom true)
         responder# (future
                      (while @running#
                        (let [result# (core/await-command dev-env# 50)]
                          (when (= :ok (:status result#))
                            (let [cmd# (:command result#)]
                              (when (= :register-hook (:command cmd#))
                                (core/send-response
                                 dev-env#
                                 (:command-id cmd#)
                                 {:status :ok})))))))]
     (try
       (Thread/sleep 30)  ; Let responder start
       ~@body
       (finally
         (reset! running# false)
         (deref responder# 100 nil)))))

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
        ;; ID format from generic registry: "dev-env-123"
        (is (re-matches #"dev-env-\d+" dev-env-id))
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
      (let [result (core/send-response-by-id
                    "unknown"
                    (java.util.UUID/randomUUID)
                    nil)]
        (is (= :error (:status result)))
        (is (string? (:message result)))))

    (testing "send-hook-event-by-id returns error for unknown id"
      (let [result (core/send-hook-event-by-id
                    "unknown" :on-close "s1" :user-exit)]
        (is (= :error (:status result)))
        (is (string? (:message result)))))

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
        (is
         (false? (core/send-response dev-env (java.util.UUID/randomUUID) "x")))
        (core/shutdown dev-env)))))

(deftest register-hook-test
  (testing "register-hook"
    (testing "stores hook and returns uuid"
      (let [dev-env (core/make-emacs-dev-env)
            callback (fn [_] :called)]
        (with-register-hook-responder dev-env
          (let [hook-id (protocol/register-hook
                         dev-env "s1" :on-close callback)]
            (is (uuid? hook-id))
            (let [hook (get-in @(:state dev-env) [:hooks hook-id])]
              (is (= :on-close (:type hook)))
              (is (= callback (:callback hook))))))
        (core/shutdown dev-env)))
    (testing "replaces existing hook for same session and type"
      (let [dev-env (core/make-emacs-dev-env)
            cb1 (fn [_] :first)
            cb2 (fn [_] :second)]
        (with-register-hook-responder dev-env
          (let [id1 (protocol/register-hook dev-env "s1" :on-close cb1)
                id2 (protocol/register-hook dev-env "s1" :on-close cb2)
                hooks (:hooks @(:state dev-env))]
            (is (not= id1 id2))
            (is (= 1 (count hooks))
                "only one hook for same session+type")
            (is (nil? (get hooks id1))
                "old hook removed")
            (is (= cb2 (:callback (get hooks id2)))
                "new hook has latest callback")))
        (core/shutdown dev-env)))
    (testing "allows different sessions to have hooks"
      (let [dev-env (core/make-emacs-dev-env)]
        (with-register-hook-responder dev-env
          (protocol/register-hook dev-env "s1" :on-close (fn [_]))
          (protocol/register-hook dev-env "s2" :on-close (fn [_]))
          (is (= 2 (count (:hooks @(:state dev-env))))))
        (core/shutdown dev-env)))))

(deftest send-hook-event-test
  (testing "send-hook-event"
    (testing "invokes matching hooks with context"
      (let [dev-env (core/make-emacs-dev-env)
            received (atom nil)
            callback (fn [ctx] (reset! received ctx))]
        (with-register-hook-responder dev-env
          (protocol/register-hook dev-env "sess-1" :on-close callback))
        (core/send-hook-event dev-env :on-close "sess-1" :user-exit)
        (is (= "sess-1" (:session-id @received)))
        (is (= :user-exit (:reason @received)))
        (is (inst? (:timestamp @received)))
        (core/shutdown dev-env)))
    (testing "only invokes hooks for the matching session-id"
      (let [dev-env (core/make-emacs-dev-env)
            s1-called (atom false)
            s2-called (atom false)]
        (with-register-hook-responder dev-env
          (protocol/register-hook dev-env "s1" :on-close
                                  (fn [_] (reset! s1-called true)))
          (protocol/register-hook dev-env "s2" :on-close
                                  (fn [_] (reset! s2-called true))))
        (core/send-hook-event dev-env :on-close "s1" :user-exit)
        (is @s1-called "s1 hook should fire")
        (is (not @s2-called) "s2 hook should not fire")
        (core/shutdown dev-env)))
    (testing "removes hooks after invocation"
      (let [dev-env (core/make-emacs-dev-env)
            call-count (atom 0)]
        (with-register-hook-responder dev-env
          (protocol/register-hook dev-env "s1" :on-close
                                  (fn [_] (swap! call-count inc))))
        (core/send-hook-event dev-env :on-close "s1" :user-exit)
        (is (= 1 @call-count))
        (is (empty? (:hooks @(:state dev-env)))
            "hooks removed after invocation")
        ;; Second send should be a no-op
        (core/send-hook-event dev-env :on-close "s1" :user-exit)
        (is (= 1 @call-count) "hook not called twice")
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
            dev-env-id (generic-registry/register! dev-env :emacs {})]
        (is (= [{:dev-env-id dev-env-id
                 :type :emacs
                 :connected? false}]
               (core/list-dev-envs)))
        (core/shutdown dev-env)
        (generic-registry/unregister! dev-env-id)))))

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
                          (let [result (core/await-command-by-id
                                        dev-env-id
                                        100)]
                            (when (= :ok (:status result))
                              (let [cmd (:command result)]
                                (when (= :ping (:command cmd))
                                  (core/send-response-by-id
                                   dev-env-id
                                   (:command-id cmd)
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
                          (let [result (core/await-command-by-id
                                        dev-env-id
                                        100)]
                            (when (= :ok (:status result))
                              (let [cmd (:command result)]
                                (when (= :ping (:command cmd))
                                  (core/send-response-by-id
                                   dev-env-id
                                   (:command-id cmd)
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
    (testing
     "routes responses to correct callers when multiple commands in flight"
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
    (testing "throwing hook does not crash send-hook-event"
      (let [dev-env (core/make-emacs-dev-env)]
        (with-register-hook-responder dev-env
          (protocol/register-hook dev-env "s1" :on-close
                                  (fn [_] (throw (ex-info "Hook error" {})))))
        (is (true? (core/send-hook-event dev-env :on-close "s1" :user-exit)))
        (core/shutdown dev-env)))
    (testing "error in one session's hook does not affect another session"
      (let [dev-env (core/make-emacs-dev-env)
            s2-called (atom false)]
        (with-register-hook-responder dev-env
          (protocol/register-hook dev-env "s1" :on-close
                                  (fn [_] (throw (ex-info "Hook error" {}))))
          (protocol/register-hook dev-env "s2" :on-close
                                  (fn [_] (reset! s2-called true))))
        (is (true? (core/send-hook-event dev-env :on-close "s1" :user-exit)))
        (is (true? (core/send-hook-event dev-env :on-close "s2" :user-exit)))
        (is @s2-called "s2 hook should fire despite s1 error")
        (core/shutdown dev-env)))))

;;; Registry Cleanup Tests

(deftest registry-cleanup-test
  ;; Verify dev-env is properly removed from registry on unregister
  ;; and all resources are cleaned up.
  (testing "registry cleanup"
    (testing "removes dev-env from registry on unregister"
      (generic-registry/clear!)
      (let [dev-env-id (core/register-emacs-dev-env)]
        (is (some? (core/get-dev-env dev-env-id)))
        (is (= 1 (count (generic-registry/list-dev-envs))))
        (core/unregister-emacs-dev-env dev-env-id)
        (is (nil? (core/get-dev-env dev-env-id)))
        (is (= 0 (count (generic-registry/list-dev-envs))))))

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

;;; Generic Registry Integration Tests

(deftest generic-registry-integration-test
  ;; Verify EmacsDevEnv registration integrates with generic dev-env registry.
  ;; This enables generic resolvers to discover and query emacs dev-envs.
  (testing "generic registry integration"
    (generic-registry/clear!)
    (try
      (testing "register-emacs-dev-env adds to generic registry"
        (let [dev-env-id (core/register-emacs-dev-env)
              generic-entry (generic-registry/get-dev-env-entry dev-env-id)]
          (is (some? generic-entry)
              "dev-env should appear in generic registry")
          (is (= :emacs (:type generic-entry))
              "generic registry entry should have :emacs type")
          (is (some? (:dev-env generic-entry))
              "generic registry entry should include dev-env instance")
          (is (= (core/get-dev-env dev-env-id)
                 (:dev-env generic-entry))
              "should reference same instance")
          (core/unregister-emacs-dev-env dev-env-id)))

      (testing "unregister-emacs-dev-env removes from generic registry"
        (let [dev-env-id (core/register-emacs-dev-env)]
          (is (some? (generic-registry/get-dev-env dev-env-id)))
          (core/unregister-emacs-dev-env dev-env-id)
          (is (nil? (generic-registry/get-dev-env dev-env-id))
              "dev-env should be removed from generic registry")))

      (testing "list-dev-envs shows emacs dev-env in generic registry"
        (let [dev-env-id (core/register-emacs-dev-env)
              listed (generic-registry/list-dev-envs)
              entry (first (filter #(= dev-env-id (:dev-env/id %)) listed))]
          (is (some? entry)
              "emacs dev-env should appear in list")
          (is (= :emacs (:type entry)))
          (core/unregister-emacs-dev-env dev-env-id)))

      (testing "generic resolvers can query emacs dev-env"
        (let [dev-env-id (core/register-emacs-dev-env)
              dev-env (generic-registry/get-dev-env dev-env-id)]
          (is (satisfies? protocol/DevEnv dev-env)
              "retrieved dev-env should satisfy DevEnv protocol")
          (is (protocol/connected? dev-env)
              "dev-env should report connected")
          (core/unregister-emacs-dev-env dev-env-id)))

      (finally
        (generic-registry/clear!)))))

;;; Session Query Tests

(deftest query-sessions-by-id-test
  ;; Verify query-sessions-by-id validates dev-env and queries
  ;; active sessions via pathom graph.
  (resolvers/register-resolvers!)
  (testing "query-sessions-by-id"
    (testing "returns error for unknown dev-env-id"
      (let [result (core/query-sessions-by-id "nonexistent")]
        (is (= :error (:status result)))
        (is (string? (:message result)))))

    (testing "returns sessions from statechart engine"
      (let [dev-env-id (core/register-emacs-dev-env)
            result (core/query-sessions-by-id dev-env-id)]
        (is (= :ok (:status result)))
        (is (vector? (:sessions result)))
        (core/unregister-emacs-dev-env dev-env-id)))))

;;; Notification Tests

(deftest notification-test
  ;; Verify fire-and-forget notifications are delivered to Emacs
  ;; without requiring a response.
  (resolvers/register-resolvers!)
  (testing "notifications"
    (testing "are delivered via await-command"
      (let [dev-env-id (core/register-emacs-dev-env)]
        (core/notify-sessions-changed! (core/get-dev-env dev-env-id))
        (let [result (core/await-command-by-id dev-env-id 1000)
              cmd (:command result)]
          (is (= :ok (:status result)))
          (is (= :notify-sessions-changed (:command cmd)))
          (is (true? (:notification cmd)))
          (is (vector? (get-in cmd [:params :sessions]))))
        (core/unregister-emacs-dev-env dev-env-id)))

    (testing "do not create pending commands"
      (let [dev-env-id (core/register-emacs-dev-env)
            dev-env (core/get-dev-env dev-env-id)]
        (core/notify-sessions-changed! dev-env)
        ;; Consume the notification
        (core/await-command-by-id dev-env-id 1000)
        ;; No pending commands should exist
        (is (empty? (:pending-commands @(:state dev-env))))
        (core/unregister-emacs-dev-env dev-env-id)))

    (testing "notify-all-sessions-changed! sends to all connected dev-envs"
      (let [id1 (core/register-emacs-dev-env)
            id2 (core/register-emacs-dev-env)]
        (core/notify-all-sessions-changed!)
        (let [r1 (core/await-command-by-id id1 1000)
              r2 (core/await-command-by-id id2 1000)]
          (is (= :ok (:status r1)))
          (is (= :ok (:status r2)))
          (is (= :notify-sessions-changed (get-in r1 [:command :command])))
          (is (= :notify-sessions-changed (get-in r2 [:command :command]))))
        (core/unregister-emacs-dev-env id1)
        (core/unregister-emacs-dev-env id2)))))

;;; Project Query Tests

(defmacro with-clean-project-registry
  "Execute body with a clean project registry, restoring state after."
  [& body]
  `(try
     (project-registry/clear!)
     ~@body
     (finally
       (project-registry/clear!))))

(deftest query-projects-by-id-test
  ;; Verify query-projects-by-id validates dev-env and returns
  ;; the project list from the registry.
  (project-resolvers/register-resolvers!)
  (testing "query-projects-by-id"
    (testing "returns error for unknown dev-env-id"
      (let [result (core/query-projects-by-id "nonexistent")]
        (is (= :error (:status result)))
        (is (string? (:message result)))))

    (testing "returns empty list when no projects registered"
      (with-clean-project-registry
        (let [dev-env-id (core/register-emacs-dev-env)
              result (core/query-projects-by-id dev-env-id)]
          (is (= :ok (:status result)))
          (is (= [] (:projects result)))
          (core/unregister-emacs-dev-env dev-env-id))))

    (testing "returns registered projects"
      (with-clean-project-registry
        (let [dev-env-id (core/register-emacs-dev-env)
              _ (project-registry/register! "/tmp" {:project/name "test-proj"})
              result (core/query-projects-by-id dev-env-id)]
          (is (= :ok (:status result)))
          (is (= 1 (count (:projects result))))
          (let [project (first (:projects result))]
            (is (= "test-proj" (:project/name project)))
            (is (string? (:project/path project))))
          (core/unregister-emacs-dev-env dev-env-id))))))

(deftest create-project-by-id-test
  ;; Verify create-project-by-id validates dev-env, creates project
  ;; via mutation, and returns correct response shape.
  (project-resolvers/register-resolvers!)
  (testing "create-project-by-id"
    (testing "returns error for unknown dev-env-id"
      (let [result (core/create-project-by-id "nonexistent" "/tmp" nil)]
        (is (= :error (:status result)))))

    (testing "creates project with name"
      (with-clean-project-registry
        (let [dev-env-id (core/register-emacs-dev-env)
              result (core/create-project-by-id dev-env-id "/tmp" "my-proj")]
          (is (= :ok (:status result)))
          (is (= "my-proj" (:project/name (:project result))))
          (core/unregister-emacs-dev-env dev-env-id))))

    (testing "creates project without name (uses dir name)"
      (with-clean-project-registry
        (let [dev-env-id (core/register-emacs-dev-env)
              result (core/create-project-by-id dev-env-id "/tmp" nil)]
          (is (= :ok (:status result)))
          (is (string? (:project/name (:project result))))
          (core/unregister-emacs-dev-env dev-env-id))))

    (testing "returns error for duplicate path"
      (with-clean-project-registry
        (let [dev-env-id (core/register-emacs-dev-env)]
          (core/create-project-by-id dev-env-id "/tmp" "first")
          (let [result (core/create-project-by-id dev-env-id "/tmp" "second")]
            (is (= :error (:status result)))
            (is (= :duplicate-path (:error result))))
          (core/unregister-emacs-dev-env dev-env-id))))))

(deftest update-project-by-id-test
  ;; Verify update-project-by-id validates dev-env, updates project
  ;; name, and returns correct response shape.
  (project-resolvers/register-resolvers!)
  (testing "update-project-by-id"
    (testing "returns error for unknown dev-env-id"
      (let [result (core/update-project-by-id "nonexistent" "/tmp" "new")]
        (is (= :error (:status result)))))

    (testing "updates project name"
      (with-clean-project-registry
        (let [dev-env-id (core/register-emacs-dev-env)]
          (core/create-project-by-id dev-env-id "/tmp" "old-name")
          (let [result (core/update-project-by-id dev-env-id "/tmp" "new-name")]
            (is (= :ok (:status result)))
            (is (= "new-name" (:project/name (:project result)))))
          (core/unregister-emacs-dev-env dev-env-id))))

    (testing "returns error for non-existent project"
      (with-clean-project-registry
        (let [dev-env-id (core/register-emacs-dev-env)
              result (core/update-project-by-id
                      dev-env-id "/nonexistent" "name")]
          (is (= :error (:status result)))
          (core/unregister-emacs-dev-env dev-env-id))))))

(deftest delete-project-by-id-test
  ;; Verify delete-project-by-id validates dev-env, removes project,
  ;; and returns correct response shape.
  (project-resolvers/register-resolvers!)
  (testing "delete-project-by-id"
    (testing "returns error for unknown dev-env-id"
      (let [result (core/delete-project-by-id "nonexistent" "/tmp")]
        (is (= :error (:status result)))))

    (testing "deletes existing project"
      (with-clean-project-registry
        (let [dev-env-id (core/register-emacs-dev-env)]
          (core/create-project-by-id dev-env-id "/tmp" "del-proj")
          (let [result (core/delete-project-by-id dev-env-id "/tmp")]
            (is (= :ok (:status result)))
            (is (= "del-proj" (:project/name (:project result)))))
          ;; Verify it's gone
          (let [list-result (core/query-projects-by-id dev-env-id)]
            (is (= [] (:projects list-result))))
          (core/unregister-emacs-dev-env dev-env-id))))

    (testing "returns error for non-existent project"
      (with-clean-project-registry
        (let [dev-env-id (core/register-emacs-dev-env)
              result (core/delete-project-by-id dev-env-id "/nonexistent")]
          (is (= :error (:status result)))
          (core/unregister-emacs-dev-env dev-env-id))))))
