(ns task-conductor.claude-cli-test.resolvers-integration-test
  "Integration tests for Claude CLI resolvers using real Claude CLI.

  These tests verify the full EQL query flow: invoke!, query status,
  wait for completion, query final result.

  Tests require Claude CLI to be installed and configured.
  They are excluded from default test runs.

  To run integration tests:
    clj -M:test integration

  To run all tests including integration:
    clj -M:test unit integration"
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.claude-cli.registry :as registry]
   [task-conductor.claude-cli.resolvers :as resolvers]
   [task-conductor.claude-cli-test.test-utils :refer [with-clean-state]]
   [task-conductor.pathom-graph.interface :as graph]))

;; Tests verify the full integration: EQL mutations trigger CLI invocation,
;; resolver queries return status and results, and the pathom graph
;; correctly wires together the operations.

(deftest ^:integration invoke-and-query-result-test
  ;; Tests the full EQL flow: invoke returns id immediately, query shows
  ;; pending initially, query shows complete with events after completion.
  (testing "invoke! and invocation-result"
    (testing "with real Claude CLI"
      (testing "full flow: invoke → pending → complete"
        (with-clean-state
          ;; Invoke with --version flag (fast, no API call)
          (let [invoke-result (graph/query
                               [`(resolvers/invoke!
                                  {:claude-cli/prompt "--version"})])
                id (get-in invoke-result [`resolvers/invoke!
                                          :claude-cli/invocation-id])]
            (is (uuid? id) "invoke! should return uuid")

            ;; Query initial status - may be pending or already complete
            (let [status-result (graph/query
                                 [{[:claude-cli/invocation-id id]
                                   [:claude-cli/status]}])
                  initial-status (get-in status-result
                                         [[:claude-cli/invocation-id id]
                                          :claude-cli/status])]
              (is (#{:pending :complete} initial-status)
                  "initial status should be pending or complete"))

            ;; Wait for completion and query full result
            (let [entry (registry/get-invocation id)
                  _ (deref (:result-promise (:handle entry)) 10000 :timeout)
                  final-result (graph/query
                                [{[:claude-cli/invocation-id id]
                                  [:claude-cli/status
                                   :claude-cli/exit-code
                                   :claude-cli/events]}])
                  response (get final-result [:claude-cli/invocation-id id])]
              (is (= :complete (:claude-cli/status response))
                  "final status should be complete")
              (is (= 0 (:claude-cli/exit-code response))
                  "exit code should be 0")
              (is (vector? (:claude-cli/events response))
                  "events should be a vector"))))))))

(deftest ^:integration cancel-running-invocation-test
  ;; Tests that cancel! mutation correctly terminates a running invocation
  ;; and updates status to cancelled.
  (testing "cancel! mutation"
    (testing "with real Claude CLI"
      (testing "cancels running invocation and updates status"
        (with-clean-state
          ;; Invoke with a long-running prompt
          (let [invoke-result (graph/query
                               [`(resolvers/invoke!
                                  {:claude-cli/prompt "count 1 to 100"})])
                id (get-in invoke-result [`resolvers/invoke!
                                          :claude-cli/invocation-id])]
            (is (uuid? id))

            ;; Brief delay to let process start
            (Thread/sleep 200)

            ;; Cancel the invocation
            (let [cancel-result (graph/query
                                 [`(resolvers/cancel!
                                    {:claude-cli/invocation-id ~id})])
                  cancel-response (get cancel-result `resolvers/cancel!)]
              (is (= id (:claude-cli/invocation-id cancel-response))
                  "cancel should return the invocation id"))

            ;; Query status should show cancelled
            (let [status-result (graph/query
                                 [{[:claude-cli/invocation-id id]
                                   [:claude-cli/status]}])
                  status (get-in status-result
                                 [[:claude-cli/invocation-id id]
                                  :claude-cli/status])]
              (is (= :cancelled status)
                  "status should be cancelled after cancel!"))))))))
