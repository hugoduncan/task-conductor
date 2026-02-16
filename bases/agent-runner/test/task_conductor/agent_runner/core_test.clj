(ns task-conductor.agent-runner.core-test
  "Integration tests for agent-runner using nullable infrastructure.

  These tests verify the agent-runner bootstrap, entry points, and
  status functions without real CLI processes or mcp-tasks subprocess.

  Test coverage:
  - bootstrap! loads resolvers and registers transition listener
  - run-story! starts session and returns session-id + state
  - run-task! starts session and returns session-id + state
  - run-task! with failing task returns error map
  - status returns current-state and history for active session"
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.core :as agent-runner]
   [task-conductor.claude-cli.interface :as claude-cli]
   [task-conductor.dev-env.registry :as dev-env-registry]
   [task-conductor.dev-env.resolvers :as dev-env-resolvers]
   [task-conductor.mcp-tasks.interface :as mcp-tasks]
   [task-conductor.mcp-tasks.resolvers :as mcp-tasks-resolvers]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.execute :as execute]
   [task-conductor.project.registry :as registry]
   [task-conductor.project.resolvers :as resolvers]
   [task-conductor.statechart-engine.core :as engine]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.resolvers :as engine-resolvers]))

;;; Test Infrastructure

(defmacro with-agent-runner-state
  "Execute body with clean state for agent-runner tests.
  Resets all registries, graph, engine, and transition listeners.
  Registers all required resolvers and statecharts."
  [& body]
  `(try
     (engine/reset-engine!)
     (graph/reset-graph!)
     (registry/clear!)
     (dev-env-registry/clear!)
     (engine-resolvers/reset-dev-env-hooks!)
     (resolvers/reset-skill-threads!)
     (sc/remove-transition-listener! ::agent-runner/transition-log)
     (execute/register-statecharts!)
     (engine-resolvers/register-resolvers!)
     (dev-env-resolvers/register-resolvers!)
     (mcp-tasks-resolvers/register-resolvers!)
     (resolvers/register-resolvers!)
     ~@body
     (finally
       (resolvers/await-skill-threads!)
       (resolvers/reset-skill-threads!)
       (sc/remove-transition-listener! ::agent-runner/transition-log)
       (engine/reset-engine!)
       (graph/reset-graph!)
       (registry/clear!)
       (dev-env-registry/clear!)
       (engine-resolvers/reset-dev-env-hooks!))))

;;; Test Data Builders

(defn- make-task-response
  [overrides]
  {:task (merge {:type :task
                 :status :open
                 :meta nil
                 :pr-num nil
                 :code-reviewed nil}
                overrides)
   :metadata {}})

(defn- make-work-on-response
  [overrides]
  (merge {:worktree-path "/test"
          :branch-name "test-branch"
          :task-id 0
          :title "Test task"}
         overrides))

(defn- task-responses
  [task-overrides]
  {:work-on [(make-work-on-response {})]
   :show [(make-task-response task-overrides)]
   :why-blocked [{:blocked-by [] :blocking-reason nil}]})

;;; Bootstrap Tests

(deftest bootstrap-test
  ;; Verify bootstrap! loads resolvers, registers transition listener,
  ;; and reports graph operational status.
  (testing "bootstrap!"
    (testing "returns operational graph and loaded namespaces"
      (with-agent-runner-state
        (let [result (agent-runner/bootstrap!)]
          (is (true? (:graph-operational? result)))
          (is (seq (:namespaces result))))))

    (testing "registers transition listener"
      (with-agent-runner-state
        (agent-runner/bootstrap!)
        ;; Verify listener was registered by checking it can be removed
        ;; (remove returns the key if it existed)
        (let [removed (sc/remove-transition-listener!
                       ::agent-runner/transition-log)]
          (is (= ::agent-runner/transition-log removed)))))))

;;; run-story! Tests

(deftest run-story-test
  ;; Verify run-story! starts a statechart session via execute! and sends
  ;; the initial-state event. Mirrors run-task! but exercises the separate
  ;; entry point that will diverge when story execution adds child-task
  ;; orchestration.
  (testing "run-story!"
    (testing "starts session and returns session-id + state"
      (with-agent-runner-state
        (let [mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses
                                         {:type :story})})
              cli-nullable (claude-cli/make-nullable)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (agent-runner/run-story! "/test" 50)]
                (is (string? (:session-id result))
                    "returns a session-id string")
                (is (some? (:state result))
                    "returns non-nil state")
                (is (nil? (:error result))
                    "returns no error")))))))))

;;; run-task! Tests

(deftest run-task-test
  ;; Verify run-task! starts a statechart session via execute! and sends
  ;; the initial-state event. Contracts: returns session-id, state, no error.
  (testing "run-task!"
    (testing "starts session for unrefined task"
      (with-agent-runner-state
        (let [mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses {})})
              cli-nullable (claude-cli/make-nullable)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (agent-runner/run-task! "/test" 100)]
                (is (string? (:session-id result))
                    "returns a session-id string")
                (is (some? (:state result))
                    "returns non-nil state")
                (is (nil? (:error result))
                    "returns no error")))))))

    (testing "starts session for refined task"
      (with-agent-runner-state
        (let [mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses
                                         {:meta {:refined "true"}})})
              cli-nullable (claude-cli/make-nullable)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (agent-runner/run-task! "/test" 101)]
                (is (string? (:session-id result)))
                (is (contains? (:state result) :refined))))))))

    (testing "returns error when execute! fails"
      (with-agent-runner-state
        (let [;; work-on returns an error
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             {:work-on [{:error :not-found
                                         :message "Task not found"}]}})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (agent-runner/run-task! "/test" 999)]
                (is (nil? (:session-id result))
                    "no session-id on error")
                (is (nil? (:state result))
                    "no state on error")
                (is (some? (:error result))
                    "returns error map")))))))))

;;; status Tests

(deftest status-test
  ;; Verify status returns current-state and history for an active session.
  (testing "status"
    (testing "returns current-state and history for active session"
      (with-agent-runner-state
        (let [mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses {})})
              cli-nullable (claude-cli/make-nullable)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [run-result (agent-runner/run-task! "/test" 200)
                    session-id (:session-id run-result)
                    s (agent-runner/status session-id)]
                (is (= session-id (:session-id s)))
                (is (set? (:current-state s))
                    "current-state is a set of keywords")
                (is (vector? (:history s))
                    "history is a vector")))))))))
