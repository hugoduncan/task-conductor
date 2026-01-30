(ns task-conductor.claude-cli-test.resolvers-test
  ;; Tests for invoke! and cancel! mutations.
  ;; Contracts: invoke returns id immediately, cancel updates status,
  ;; cancel on unknown id returns error.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.claude-cli.interface :as interface]
   [task-conductor.claude-cli.registry :as registry]
   [task-conductor.claude-cli.resolvers :as resolvers]
   [task-conductor.claude-cli-test.test-utils :refer [with-clean-state]]
   [task-conductor.pathom-graph.interface :as graph]))

(defn mock-handle
  "Create a mock handle for testing."
  []
  {:process :mock-process
   :result-promise (promise)})

;;; invoke! mutation tests

(deftest invoke-mutation-test
  (testing "invoke! mutation"
    (testing "returns invocation id immediately"
      (with-clean-state
        (with-redefs [interface/invoke (fn [_opts] (mock-handle))]
          (let [result (graph/query
                        [`(resolvers/invoke!
                           {:claude-cli/prompt "test"})])
                response (get result `resolvers/invoke!)]
            (is (uuid? (:claude-cli/invocation-id response)))))))

    (testing "stores handle in registry with pending status"
      (with-clean-state
        (with-redefs [interface/invoke (fn [_opts] (mock-handle))]
          (let [result (graph/query
                        [`(resolvers/invoke!
                           {:claude-cli/prompt "test"})])
                id (get-in result [`resolvers/invoke! :claude-cli/invocation-id])
                entry (registry/get-invocation id)]
            (is (some? entry))
            (is (= :pending (:status entry)))
            (is (some? (:handle entry)))))))

    (testing "passes prompt to interface/invoke"
      (with-clean-state
        (let [captured-opts (atom nil)]
          (with-redefs [interface/invoke (fn [opts]
                                           (reset! captured-opts opts)
                                           (mock-handle))]
            (graph/query
             [`(resolvers/invoke!
                {:claude-cli/prompt "my prompt"})])
            (is (= "my prompt" (:prompt @captured-opts)))))))

    (testing "passes dir to interface/invoke when provided"
      (with-clean-state
        (let [captured-opts (atom nil)]
          (with-redefs [interface/invoke (fn [opts]
                                           (reset! captured-opts opts)
                                           (mock-handle))]
            (graph/query
             [`(resolvers/invoke!
                {:claude-cli/prompt "test"
                 :claude-cli/dir "/tmp"})])
            (is (= "/tmp" (:dir @captured-opts)))))))))

;;; cancel! mutation tests

(deftest cancel-mutation-test
  (testing "cancel! mutation"
    (testing "updates status to cancelled"
      (with-clean-state
        (let [handle (mock-handle)
              id (registry/create-invocation! handle)]
          (with-redefs [interface/cancel! (fn [_h] true)]
            (let [result (graph/query
                          [`(resolvers/cancel!
                             {:claude-cli/invocation-id ~id})])
                  response (get result `resolvers/cancel!)
                  entry (registry/get-invocation id)]
              (is (= id (:claude-cli/invocation-id response)))
              (is (= :cancelled (:status entry))))))))

    (testing "calls interface/cancel! with handle"
      (with-clean-state
        (let [handle (mock-handle)
              id (registry/create-invocation! handle)
              captured-handle (atom nil)]
          (with-redefs [interface/cancel! (fn [h]
                                            (reset! captured-handle h)
                                            true)]
            (graph/query
             [`(resolvers/cancel!
                {:claude-cli/invocation-id ~id})])
            (is (= handle @captured-handle))))))

    (testing "returns error for unknown invocation id"
      (with-clean-state
        (let [fake-id (random-uuid)
              result (graph/query
                      [`(resolvers/cancel!
                         {:claude-cli/invocation-id ~fake-id})])
              response (get result `resolvers/cancel!)]
          (is (= :not-found (:claude-cli/error response)))
          (is (= fake-id (:claude-cli/invocation-id response))))))))

;;; invocation-result resolver tests

(deftest invocation-result-test
  (testing "invocation-result resolver"
    (testing "returns :pending status for unrealized promise"
      (with-clean-state
        (let [handle (mock-handle)
              id (registry/create-invocation! handle)
              result (graph/query [{[:claude-cli/invocation-id id]
                                    [:claude-cli/status]}])
              response (get result [:claude-cli/invocation-id id])]
          (is (= :pending (:claude-cli/status response))))))

    (testing "returns :complete status with result when promise realized"
      (with-clean-state
        (let [p (promise)
              handle {:process :mock :result-promise p}
              id (registry/create-invocation! handle)
              _ (deliver p {:exit-code 0 :events [{:type "end"}]})
              result (graph/query [{[:claude-cli/invocation-id id]
                                    [:claude-cli/status
                                     :claude-cli/exit-code
                                     :claude-cli/events]}])
              response (get result [:claude-cli/invocation-id id])]
          (is (= :complete (:claude-cli/status response)))
          (is (= 0 (:claude-cli/exit-code response)))
          (is (= [{:type "end"}] (:claude-cli/events response))))))

    (testing "returns :error status with error details when result contains error"
      (with-clean-state
        (let [p (promise)
              handle {:process :mock :result-promise p}
              id (registry/create-invocation! handle)
              _ (deliver p {:exit-code nil :error :timeout})
              result (graph/query [{[:claude-cli/invocation-id id]
                                    [:claude-cli/status
                                     :claude-cli/exit-code
                                     :claude-cli/error]}])
              response (get result [:claude-cli/invocation-id id])]
          (is (= :error (:claude-cli/status response)))
          (is (nil? (:claude-cli/exit-code response)))
          (is (= :timeout (:claude-cli/error response))))))

    (testing "returns :cancelled status for cancelled invocation"
      (with-clean-state
        (let [handle (mock-handle)
              id (registry/create-invocation! handle)]
          (registry/update-invocation! id {:status :cancelled})
          (let [result (graph/query [{[:claude-cli/invocation-id id]
                                      [:claude-cli/status]}])
                response (get result [:claude-cli/invocation-id id])]
            (is (= :cancelled (:claude-cli/status response)))))))

    (testing "returns :not-found status and error for unknown invocation id"
      (with-clean-state
        (let [fake-id (random-uuid)
              result (graph/query [{[:claude-cli/invocation-id fake-id]
                                    [:claude-cli/status
                                     :claude-cli/error]}])
              response (get result [:claude-cli/invocation-id fake-id])]
          (is (= :not-found (:claude-cli/status response)))
          (is (= :not-found (:claude-cli/error response))))))))
