(ns task-conductor.emacs-dev-env.resolvers-test
  ;; Verify EQL resolvers for dev-env selection and operations.
  ;; Tests that resolvers register correctly and return expected data.
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [task-conductor.emacs-dev-env.core :as core]
   [task-conductor.emacs-dev-env.resolvers :as resolvers]
   [task-conductor.pathom-graph.interface :as graph]))

(use-fixtures :each
  (fn [f]
    (graph/reset-graph!)
    (resolvers/register-resolvers!)
    (f)
    (graph/reset-graph!)))

(deftest available-dev-envs-resolver-test
  (testing "available-dev-envs resolver"
    (testing "returns empty list when none registered"
      (let [result (graph/query [:dev-env/available])]
        (is (= [] (:dev-env/available result)))))

    (testing "returns registered dev-envs"
      (let [dev-env-id (core/register-emacs-dev-env)
            result (graph/query [:dev-env/available])]
        (is (= [{:dev-env-id dev-env-id
                 :type :emacs
                 :connected? true}]
               (:dev-env/available result)))
        (core/unregister-emacs-dev-env dev-env-id)))))

(deftest healthy-dev-envs-resolver-test
  ;; The healthy resolver pings dev-envs, so without a responder
  ;; it will timeout and return empty.
  (testing "healthy-dev-envs resolver"
    (testing "returns empty when no dev-envs respond"
      (let [dev-env-id (core/register-emacs-dev-env)]
        ;; Short timeout, no responder
        (with-redefs [core/default-ping-timeout-ms 50]
          (let [result (graph/query [:dev-env/healthy])]
            (is (= [] (:dev-env/healthy result)))))
        (core/unregister-emacs-dev-env dev-env-id)))))

(deftest selected-dev-env-resolver-test
  (testing "selected-dev-env resolver"
    (testing "returns nil when no healthy dev-envs"
      (with-redefs [core/default-ping-timeout-ms 50]
        (let [result (graph/query [:dev-env/selected])]
          (is (nil? (:dev-env/selected result))))))))

(deftest dev-env-by-id-resolver-test
  (testing "dev-env-by-id resolver"
    (testing "returns nil for unknown id"
      (let [result (graph/query {:dev-env/id "unknown"}
                                [:dev-env/instance :dev-env/type :dev-env/connected?])]
        (is (nil? (:dev-env/instance result)))
        (is (nil? (:dev-env/type result)))
        (is (false? (:dev-env/connected? result)))))

    (testing "returns dev-env for valid id"
      (let [dev-env-id (core/register-emacs-dev-env)
            result (graph/query {:dev-env/id dev-env-id}
                                [:dev-env/instance :dev-env/type :dev-env/connected?])]
        (is (some? (:dev-env/instance result)))
        (is (= :emacs (:dev-env/type result)))
        (is (true? (:dev-env/connected? result)))
        (core/unregister-emacs-dev-env dev-env-id)))))

(deftest dev-env-health-resolver-test
  (testing "dev-env-health resolver"
    (testing "returns error for unknown id"
      (let [result (graph/query {:dev-env/id "unknown"} [:dev-env/health])]
        (is (= {:status :error :message "Dev-env not found: unknown"}
               (:dev-env/health result)))))

    (testing "returns timeout for unresponsive dev-env"
      (let [dev-env-id (core/register-emacs-dev-env)]
        (with-redefs [core/default-ping-timeout-ms 50]
          (let [result (graph/query {:dev-env/id dev-env-id} [:dev-env/health])]
            (is (= {:status :timeout} (:dev-env/health result)))))
        (core/unregister-emacs-dev-env dev-env-id)))))

(deftest dev-env-start-session-mutation-test
  (testing "dev-env-start-session! mutation"
    (testing "returns error for unknown dev-env"
      (let [result (graph/query
                    [`(resolvers/dev-env-start-session!
                       {:dev-env/id "unknown"
                        :dev-env/session-id "s1"})])]
        (is (= {:error :not-found :message "Dev-env not found: unknown"}
               (:dev-env/session-result (get result `resolvers/dev-env-start-session!))))))

    (testing "queues start-session command"
      (let [dev-env-id (core/register-emacs-dev-env)]
        ;; Start mutation in a future (will block waiting for response)
        (let [mutation-future
              (future
                (graph/query
                 [`(resolvers/dev-env-start-session!
                    {:dev-env/id ~dev-env-id
                     :dev-env/session-id "s1"
                     :dev-env/opts {:dir "/tmp"}})]))
              ;; Wait for command to be queued
              _ (Thread/sleep 50)
              result (core/await-command-by-id dev-env-id 100)
              cmd (:command result)]
          (is (= :ok (:status result)))
          (is (= :start-session (:command cmd)))
          (is (= "s1" (get-in cmd [:params :session-id])))
          ;; Send response
          (core/send-response-by-id dev-env-id (:command-id cmd) {:handle :test})
          (let [mutation-result @mutation-future]
            (is (= {:handle :test}
                   (:dev-env/session-result
                    (get mutation-result `resolvers/dev-env-start-session!))))))
        (core/unregister-emacs-dev-env dev-env-id)))))
