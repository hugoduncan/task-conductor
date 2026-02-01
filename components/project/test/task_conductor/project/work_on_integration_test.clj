(ns task-conductor.project.work-on-integration-test
  "Integration tests for work-on! EQL mutation using Nullable infrastructure.

  These tests exercise the statechart lifecycle without subprocess execution:
  - Uses claude-cli Nullable for skill invocations (no real CLI processes)
  - Uses mock fetchers for task data (following existing test patterns)
  - Uses NoOpDevEnv for dev-env escalation
  - Asserts on statechart states via current-state, not interaction counts

  Test coverage:
  - Session startup and initial state derivation
  - Skill invocation via entry actions using claude-cli Nullable
  - Error path: skill failure triggers escalation to dev-env
  - State transitions through the statechart lifecycle"
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.claude-cli.interface :as claude-cli]
   [task-conductor.dev-env.protocol :as dev-env-protocol]
   [task-conductor.dev-env.registry :as dev-env-registry]
   [task-conductor.dev-env.resolvers :as dev-env-resolvers]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.registry :as registry]
   [task-conductor.project.resolvers :as resolvers]
   [task-conductor.project.work-on :as work-on]
   [task-conductor.statechart-engine.core :as engine]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.resolvers :as engine-resolvers]))

;;; Test Infrastructure

(defmacro with-integration-state
  "Execute body with clean state for integration tests.
  Resets all registries, graph, and engine state.
  Registers all required resolvers."
  [& body]
  `(do
     (engine/reset-engine!)
     (graph/reset-graph!)
     (registry/clear!)
     (dev-env-registry/clear!)
     (engine-resolvers/reset-dev-env-hooks!)
     (resolvers/reset-skill-threads!)
     ;; Register statecharts
     (work-on/register-statecharts!)
     ;; Register all resolvers
     (engine-resolvers/register-resolvers!)
     (dev-env-resolvers/register-resolvers!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         (resolvers/await-skill-threads!)
         (resolvers/reset-skill-threads!)
         (engine/reset-engine!)
         (graph/reset-graph!)
         (registry/clear!)
         (dev-env-registry/clear!)
         (engine-resolvers/reset-dev-env-hooks!)))))

;;; Test Data Builders (following existing patterns from resolvers_test.clj)

(defn make-task
  "Create task data with sensible defaults."
  ([] (make-task {}))
  ([overrides]
   (merge {:task/type :task
           :task/status :open
           :task/meta nil
           :task/pr-num nil
           :task/code-reviewed nil}
          overrides)))

(defn make-child
  "Create child task data with defaults."
  ([] (make-child {}))
  ([overrides]
   (merge {:task/status :open} overrides)))

(defmacro with-mock-fetchers
  "Execute body with mocked fetch-task and fetch-children.
  task-fn: (fn [dir id] task-map)
  children-fn: (fn [dir parent-id] [child-maps])"
  [task-fn children-fn & body]
  `(with-redefs [resolvers/fetch-task ~task-fn
                 resolvers/fetch-children ~children-fn]
     ~@body))

;;; Session Startup Tests

(deftest session-startup-test
  ;; Verify work-on! correctly starts a statechart session and derives
  ;; initial state. Uses mock fetchers for task data.
  (testing "session startup"
    (testing "starts session for unrefined task"
      (with-integration-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task))
          (fn [_dir _id] [])
          (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
            (let [result (graph/query [`(resolvers/work-on!
                                         {:task/project-dir "/test"
                                          :task/id 100})])
                  work-on-result (get result `resolvers/work-on!)]

              (is (string? (:work-on/session-id work-on-result)))
              (is (= :unrefined (:work-on/initial-state work-on-result)))
              (is (nil? (:work-on/error work-on-result)))

              ;; Verify session is in :idle state
              (is (contains? (sc/current-state (:work-on/session-id work-on-result))
                             :idle)))))))

    (testing "starts session for refined task"
      (with-integration-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
          (fn [_dir _id] [])
          (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
            (let [result (graph/query [`(resolvers/work-on!
                                         {:task/project-dir "/test"
                                          :task/id 101})])
                  work-on-result (get result `resolvers/work-on!)]

              (is (= :refined (:work-on/initial-state work-on-result))))))))

    (testing "starts session for task with PR"
      (with-integration-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/meta {:refined "true"}
                                     :task/pr-num 42}))
          (fn [_dir _id] [])
          (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
            (let [result (graph/query [`(resolvers/work-on!
                                         {:task/project-dir "/test"
                                          :task/id 102})])
                  work-on-result (get result `resolvers/work-on!)]

              (is (= :wait-pr-merge (:work-on/initial-state work-on-result))))))))))

;;; Skill Invocation Tests

(deftest skill-invocation-with-nullable-test
  ;; Verify that state transitions trigger entry actions which invoke skills
  ;; via claude-cli Nullable. No real CLI processes are spawned.
  (testing "skill invocation via entry action"
    (testing "invokes skill when entering :unrefined state"
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:exit-code 0 :events []})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]

          (with-mock-fetchers
            ;; Initial task is unrefined
            (fn [_dir _id] (make-task))
            (fn [_dir _id] [])
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/work-on!
                                           {:task/project-dir "/test"
                                            :task/id 200})])
                    work-on-result (get result `resolvers/work-on!)
                    session-id (:work-on/session-id work-on-result)]

                ;; Send initial state event to trigger skill
                (sc/send! session-id :unrefined)
                (is (contains? (sc/current-state session-id) :unrefined))

                ;; Wait for skill invocation
                (resolvers/await-skill-threads!)

                ;; Verify claude-cli Nullable was invoked
                (is (= 1 (count (claude-cli/invocations cli-nullable))))
                (is (= "/mcp-tasks:refine-task"
                       (:prompt (:opts (first (claude-cli/invocations cli-nullable))))))))))))))

;;; Error Path Tests

(deftest error-path-with-nullable-test
  ;; Verify skill failure triggers escalation to dev-env using Nullable.
  (testing "error path"
    (testing "transitions to :escalated on skill error"
      (with-integration-state
        (let [;; Configure cli to return error
              cli-nullable (claude-cli/make-nullable {:error :timeout})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]

          (with-mock-fetchers
            (fn [_dir _id] (make-task))
            (fn [_dir _id] [])
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/work-on!
                                           {:task/project-dir "/test"
                                            :task/id 300})])
                    work-on-result (get result `resolvers/work-on!)
                    session-id (:work-on/session-id work-on-result)]

                ;; Send initial state to trigger skill
                (sc/send! session-id :unrefined)

                ;; Wait for skill thread to process error
                (resolvers/await-skill-threads!)

                ;; Verify transitioned to :escalated
                (is (contains? (sc/current-state session-id) :escalated))

                ;; Verify dev-env was notified (start-session called)
                (let [calls @(:calls dev-env)
                      start-calls (filter #(= :start-session (:op %)) calls)]
                  (is (= 1 (count start-calls)))
                  (is (= session-id (:session-id (first start-calls)))))))))))

    (testing "error from :refined state goes to :escalated"
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:error :interrupted})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]

          (with-mock-fetchers
            (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
            (fn [_dir _id] [])
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/work-on!
                                           {:task/project-dir "/test"
                                            :task/id 301})])
                    work-on-result (get result `resolvers/work-on!)
                    session-id (:work-on/session-id work-on-result)]

                ;; Start in refined state
                (is (= :refined (:work-on/initial-state work-on-result)))

                ;; Send refined event to trigger execute skill
                (sc/send! session-id :refined)
                (resolvers/await-skill-threads!)

                ;; Should be escalated
                (is (contains? (sc/current-state session-id) :escalated))))))))))

;;; Story Tests

(deftest story-session-test
  ;; Verify work-on! handles stories correctly with children.
  (testing "story session"
    (testing "derives :has-tasks for story with incomplete children"
      (with-integration-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/type :story
                                     :task/meta {:refined "true"}}))
          (fn [_dir _id] [(make-child) (make-child {:task/status :closed})])
          (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
            (let [result (graph/query [`(resolvers/work-on!
                                         {:task/project-dir "/test"
                                          :task/id 400})])
                  work-on-result (get result `resolvers/work-on!)]

              (is (= :has-tasks (:work-on/initial-state work-on-result))))))))

    (testing "derives :awaiting-review for story with all children complete"
      (with-integration-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/type :story
                                     :task/meta {:refined "true"}}))
          (fn [_dir _id] [(make-child {:task/status :closed})
                          (make-child {:task/status :closed})])
          (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
            (let [result (graph/query [`(resolvers/work-on!
                                         {:task/project-dir "/test"
                                          :task/id 410})])
                  work-on-result (get result `resolvers/work-on!)]

              (is (= :awaiting-review (:work-on/initial-state work-on-result))))))))))

;;; State Transition Tests

(deftest state-transition-test
  ;; Verify statechart transitions work correctly.
  (testing "state transitions"
    (testing "task can transition directly to :complete from :idle"
      (with-integration-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task))
          (fn [_dir _id] [])
          (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
            (let [result (graph/query [`(resolvers/work-on!
                                         {:task/project-dir "/test"
                                          :task/id 500})])
                  session-id (:work-on/session-id (get result `resolvers/work-on!))]

              ;; Starts in :idle
              (is (contains? (sc/current-state session-id) :idle))

              ;; Can transition directly to complete from idle
              (sc/send! session-id :complete)
              (is (= #{} (sc/current-state session-id))))))))

    (testing "story transitions through :has-tasks loop"
      (with-integration-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/type :story
                                     :task/meta {:refined "true"}}))
          (fn [_dir _id] [(make-child)])
          (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
            (let [result (graph/query [`(resolvers/work-on!
                                         {:task/project-dir "/test"
                                          :task/id 510})])
                  session-id (:work-on/session-id (get result `resolvers/work-on!))]

              ;; Send has-tasks multiple times (simulating multiple children)
              (sc/send! session-id :has-tasks)
              (is (contains? (sc/current-state session-id) :has-tasks))

              (sc/send! session-id :has-tasks)
              (is (contains? (sc/current-state session-id) :has-tasks))

              ;; Transition to awaiting-review
              (sc/send! session-id :awaiting-review)
              (is (contains? (sc/current-state session-id) :awaiting-review)))))))))

;;; Nullable Infrastructure Verification

(deftest nullable-tracks-invocations-test
  ;; Verify Nullable infrastructure tracks claude-cli invocations.
  (testing "Nullable infrastructure"
    (testing "tracks claude-cli invocations during skill execution"
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:exit-code 0 :events []})]

          (with-mock-fetchers
            (fn [_dir _id] (make-task))
            (fn [_dir _id] [])
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/work-on!
                                           {:task/project-dir "/my/project"
                                            :task/id 600})])
                    session-id (:work-on/session-id (get result `resolvers/work-on!))]

                ;; Trigger skill invocation
                (sc/send! session-id :unrefined)
                (resolvers/await-skill-threads!)

                ;; Verify invocation was tracked with correct options
                (let [invs (claude-cli/invocations cli-nullable)]
                  (is (= 1 (count invs)))
                  (is (= "/my/project" (:dir (:opts (first invs)))))
                  (is (= "/mcp-tasks:refine-task" (:prompt (:opts (first invs)))))
                  (is (instance? java.time.Instant (:timestamp (first invs)))))))))))))
