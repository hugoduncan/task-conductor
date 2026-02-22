(ns task-conductor.project.execute-integration-test
  "Integration tests for execute! EQL mutation using Nullable infrastructure.

  These tests exercise the statechart lifecycle without subprocess execution:
  - Uses claude-cli Nullable for skill invocations (no real CLI processes)
  - Uses mcp-tasks Nullable for task data (no mcp-tasks CLI subprocesses)
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
   [task-conductor.mcp-tasks.interface :as mcp-tasks]
   [task-conductor.mcp-tasks.resolvers :as mcp-tasks-resolvers]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.registry :as registry]
   [task-conductor.project.resolvers :as resolvers]
   [task-conductor.project.execute :as execute]
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
     (execute/register-statecharts!)
     ;; Register resolvers
     (engine-resolvers/register-resolvers!)
     (dev-env-resolvers/register-resolvers!)
     (mcp-tasks-resolvers/register-resolvers!)
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

;;; Test Data Builders
;;; Builds responses in mcp-tasks CLI format (unnamespaced keys).
;;; The mcp-tasks resolvers add :task/ namespace via prefix-keys.

(defn make-task-response
  "Build a show-task CLI response map with sensible defaults.

  The mcp-tasks CLI returns unnamespaced keys; resolvers add :task/ prefix.
  Use this to configure task state for test scenarios.

  Options (in `overrides`):
    :id          - task ID (integer)
    :type        - :task or :story
    :status      - :open, :closed, :in-progress, etc.
    :meta        - map with :refined, etc. (nil if unrefined)
    :pr-num      - PR number if task has open PR (nil otherwise)
    :code-reviewed - timestamp if reviewed (nil otherwise)
    Any other task fields the mcp-tasks CLI returns.

  Returns {:task {...} :metadata {...}} matching CLI output format."
  ([] (make-task-response {}))
  ([overrides]
   {:task (merge {:type :task
                  :status :open
                  :title "test-task"
                  :meta nil
                  :pr-num nil
                  :code-reviewed nil}
                 overrides)
    :metadata {}}))

(defn make-blocking-response
  "Build a why-blocked CLI response map.

  Use to simulate blocking relationships between tasks.

  Options (in `overrides`):
    :blocked-by      - vector of blocking task IDs
    :blocking-reason - human-readable explanation (nil if unblocked)

  Returns {:blocked-by [...] :blocking-reason ...} matching CLI output format."
  ([] (make-blocking-response {}))
  ([overrides]
   (merge {:blocked-by []
           :blocking-reason nil}
          overrides)))

(defn make-children-response
  "Build a list-tasks CLI response for story children.

  Each child map defaults to {:status :open}, merged with provided values.
  Use to simulate stories with various child task configurations.

  Args:
    children - vector of child task maps (each will be merged with defaults)

  Returns {:tasks [...] :metadata {:total N}} matching CLI output format."
  ([] (make-children-response []))
  ([children]
   {:tasks (mapv (fn [c] (merge {:status :open} c)) children)
    :metadata {:total (count children)}}))

(defn make-work-on-response
  "Build a work-on CLI response map."
  ([] (make-work-on-response {}))
  ([overrides]
   (merge {:worktree-path "/test"
           :branch-name "test-branch"
           :task-id 0
           :title "Test task"}
          overrides)))

(defn task-responses
  "Build command-keyed responses for a task in mcp-tasks Nullable.

  Pathom queries task data via multiple resolvers (show-task, why-blocked).
  This bundles them into the command-keyed format expected by the Nullable.

  Use for simple task scenarios. For stories, use `story-responses`.

  Args:
    task-overrides - map passed to `make-task-response`

  Returns {:work-on [...] :show [...] :why-blocked [...]}
  for mcp-tasks Nullable :responses."
  ([] (task-responses {}))
  ([task-overrides]
   {:work-on [(make-work-on-response)]
    :show [(make-task-response task-overrides)]
    :why-blocked [(make-blocking-response)]}))

(defn merge-responses
  "Combine multiple command-keyed response maps for sequential queries.

  The mcp-tasks Nullable consumes responses in order. When a test triggers
  multiple queries (e.g., initial fetch then re-derive after skill), merge
  their responses so each query gets the correct data.

  Example:
    (merge-responses
      (task-responses)              ; initial unrefined state
      (task-responses {:meta {...}})) ; refined state after skill

  Returns merged map with concatenated response vectors per command."
  [& response-maps]
  (apply merge-with into response-maps))

;;; Session Startup Tests

(deftest session-startup-test
  ;; Verify execute! correctly starts a statechart session and derives
  ;; initial state. Uses mcp-tasks Nullable for task data.
  (testing "session startup"
    (testing "starts session for unrefined task"
      (with-integration-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses (task-responses)})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 100})])
                    execute-result (get result `resolvers/execute!)
                    session-id (:execute/session-id execute-result)]

                (is (string? session-id))
                (is (= :unrefined (:execute/initial-state execute-result)))
                (is (nil? (:execute/error execute-result)))

                ;; Verify session is in :idle state
                (is (contains? (sc/current-state session-id) :idle))))))))

    (testing "starts session for refined task"
      (with-integration-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses (task-responses
                                     {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 101})])
                    execute-result (get result `resolvers/execute!)]

                (is (= :refined (:execute/initial-state execute-result)))))))))

    (testing "starts session for task with PR"
      (with-integration-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses (task-responses {:meta {:refined "true"}
                                                     :pr-num 42})})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 102})])
                    execute-result (get result `resolvers/execute!)
                    initial-state (:execute/initial-state execute-result)]

                (is (= :wait-pr-merge initial-state))))))))))

;;; Skill Invocation Tests

(deftest skill-invocation-with-nullable-test
  ;; Verify that state transitions trigger entry actions which invoke skills
  ;; via claude-cli Nullable. No real CLI processes are spawned.
  (testing "skill invocation via entry action"
    (testing "invokes skill when entering :unrefined state"
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable
                            {:exit-code 0 :events []})
              ;; initial execute! + re-derive after skill
              resps (merge-responses
                     (task-responses)
                     (task-responses {:meta {:refined "true"}}))
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses resps})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]

          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query
                            [`(resolvers/execute!
                               {:task/project-dir "/test"
                                :task/id 200})])
                    execute-result (get result
                                        `resolvers/execute!)
                    session-id (:execute/session-id
                                execute-result)]

                ;; Send initial state event to trigger skill
                (sc/send! session-id :unrefined)
                (is (contains? (sc/current-state session-id) :unrefined))

                ;; Wait for skill invocation
                (resolvers/await-skill-threads!)

                ;; Verify claude-cli Nullable was invoked
                (let [invs (claude-cli/invocations cli-nullable)
                      inv-opts (:opts (first invs))
                      prompt (:prompt inv-opts)]
                  (is (= 1 (count invs)))
                  (is (= "/mcp-tasks:refine-task (MCP) 200" prompt)))))))))))

;;; Error Path Tests

(deftest error-path-with-nullable-test
  ;; Verify skill failure triggers escalation to dev-env using Nullable.
  (testing "error path"
    (testing "transitions to :escalated on skill error"
      (with-integration-state
        (let [;; Configure cli to return error
              cli-nullable (claude-cli/make-nullable {:error :timeout})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses)})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]

          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 300})])
                    execute-result (get result `resolvers/execute!)
                    session-id (:execute/session-id execute-result)]

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
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             (task-responses {:meta {:refined "true"}})})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]

          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 301})])
                    execute-result (get result `resolvers/execute!)
                    session-id (:execute/session-id execute-result)]

                ;; Start in refined state
                (is (= :refined (:execute/initial-state execute-result)))

                ;; Send refined event to trigger execute skill
                (sc/send! session-id :refined)
                (resolvers/await-skill-threads!)

                ;; Should be escalated
                (let [state (sc/current-state session-id)]
                  (is (contains? state :escalated)))))))))))

;;; Story Tests

(defn story-responses
  "Build command-keyed responses for a story with children.

  Stories require additional :list response for child task queries.
  Use to test story-specific states (:has-tasks, :done, etc.).

  Args:
    story-overrides - map passed to `make-task-response` (type forced to :story)
    children        - vector of child task maps for `make-children-response`

  Returns command-keyed map for mcp-tasks Nullable."
  [story-overrides children]
  {:work-on [(make-work-on-response)]
   :show [(make-task-response (merge {:type :story} story-overrides))]
   :why-blocked [(make-blocking-response)]
   :list [(make-children-response children)]})

(deftest story-session-test
  ;; Verify execute! handles stories correctly with children.
  (testing "story session"
    (testing "derives :has-tasks for story with incomplete children"
      (with-integration-state
        (let [mcp-nullable (mcp-tasks/make-nullable
                            {:responses (story-responses
                                         {:meta {:refined "true"}}
                                         [{:status :open}
                                          {:status :closed}])})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 400})])
                    execute-result (get result `resolvers/execute!)
                    initial-state (:execute/initial-state execute-result)]

                (is (= :has-tasks initial-state))))))))

    (testing "derives :done for story with all children complete"
      (with-integration-state
        (let [mcp-nullable (mcp-tasks/make-nullable
                            {:responses (story-responses
                                         {:meta {:refined "true"}}
                                         [{:status :closed}
                                          {:status :closed}])})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 410})])
                    execute-result (get result `resolvers/execute!)]

                (is (= :done (:execute/initial-state execute-result)))))))))))

;;; State Transition Tests

(deftest state-transition-test
  ;; Verify statechart transitions work correctly.
  (testing "state transitions"
    (testing "task can transition directly to :complete from :idle"
      (with-integration-state
        (let [mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses)})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 500})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]

                ;; Starts in :idle
                (is (contains? (sc/current-state session-id) :idle))

                ;; Can transition directly to complete from idle
                (sc/send! session-id :complete)
                ;; State is #{:complete} not #{} because on-entry
                ;; runs async /complete-story skill
                (is (contains? (sc/current-state session-id) :complete))))))))

    (testing "story progresses through :has-tasks when children complete"
      (with-integration-state
        ;; Test that story makes progress when children complete
        ;; Uses progressively closing children to simulate real workflow
        (let [story-response
              (make-task-response {:type :story :meta {:refined "true"}})
              ;; Story with code-reviewed set after review skill
              reviewed-story
              (make-task-response {:type :story
                                   :meta {:refined "true"}
                                   :code-reviewed "2024-01-01T00:00:00Z"})
              ;; Children progressively close
              three-open
              (make-children-response
               [{:status :open} {:status :open} {:status :open}])
              two-open
              (make-children-response
               [{:status :open} {:status :open} {:status :closed}])
              one-open
              (make-children-response
               [{:status :open} {:status :closed} {:status :closed}])
              all-closed
              (make-children-response
               [{:status :closed} {:status :closed} {:status :closed}])
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses {:work-on [(make-work-on-response)]
                            :show (concat
                                   (repeat
                                    10
                                    story-response)   ; during :has-tasks cycles
                                   (repeat
                                    5
                                    reviewed-story))
                            :why-blocked [(make-blocking-response)]
                            ;; Pairs of (pre-skill, on-complete)
                            ;; showing progress per cycle
                            :list (concat
                                   [three-open]  ; execute!
                                   [three-open   ; cycle 1:
                                    two-open]    ;   3->2 (progress)
                                   [two-open     ; cycle 2:
                                    one-open]    ;   2->1 (progress)
                                   [one-open     ; cycle 3:
                                    all-closed]  ;   1->0 (->done)
                                   (repeat 10 all-closed))}})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 510})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]

                ;; Send has-tasks, triggers cascade as children complete
                (sc/send! session-id :has-tasks)
                (resolvers/await-skill-threads!)
                ;; Check that we made progress through the history
                (let [hist (sc/history session-id)
                      events (set (map :event hist))]
                  ;; Should have seen :has-tasks events (children executing)
                  (is (contains? events :has-tasks)
                      "Should execute child tasks")
                  ;; Eventually progressed to done or beyond
                  (is (or (contains? events :done)
                          (contains? events :awaiting-pr)
                          (contains? events :no-progress))
                      "Should progress past :has-tasks"))))))))))

;;; Skill Task ID Prompt Tests

(deftest skill-task-id-prompt-test
  ;; Verify :args "{task-id}" is substituted into the prompt for each
  ;; skill action. Uses {:error :timeout} cli-nullable so on-skill-complete
  ;; sends :error immediately with no re-derivation, preventing cascades.
  (testing "task ID is passed in prompt for each skill action"
    (testing "execute-task-action includes task ID"
      ;; Send :refined from :idle → enters :refined → fires execute-task-action
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:error :timeout})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses
                                         {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 701})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]
                (sc/send! session-id :refined)
                (resolvers/await-skill-threads!)
                (let [invs (claude-cli/invocations cli-nullable)
                      prompt (:prompt (:opts (first invs)))]
                  (is (= 1 (count invs)))
                  (is (= "/mcp-tasks:execute-task (MCP) 701" prompt)))))))))

    (testing "review-task-action includes task ID"
      ;; Send :done from :idle → enters :done → fires review-task-action
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:error :timeout})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses
                                         {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks
            mcp-nullable
            (claude-cli/with-nullable-claude-cli
              cli-nullable
              (let [result
                    (graph/query
                     [`(resolvers/execute!
                        {:task/project-dir "/test" :task/id 702})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]
                (sc/send! session-id :done)
                (resolvers/await-skill-threads!)
                (let [invs (claude-cli/invocations cli-nullable)
                      prompt (:prompt (:opts (first invs)))]
                  (is (= 1 (count invs)))
                  (is
                   (=
                    "/mcp-tasks:review-task-implementation (MCP) 702"
                    prompt)))))))))

    (testing "create-task-pr-action includes task ID"
      ;; Send :awaiting-pr from :idle → enters :awaiting-pr
      ;; → fires create-task-pr-action
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:error :timeout})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses
                                         {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 703})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]
                (sc/send! session-id :awaiting-pr)
                (resolvers/await-skill-threads!)
                (let [invs (claude-cli/invocations cli-nullable)
                      prompt (:prompt (:opts (first invs)))]
                  (is (= 1 (count invs)))
                  (is (= "/mcp-tasks:create-task-pr (MCP) 703" prompt)))))))))

    (testing "merge-pr-action includes task ID"
      ;; :wait-pr-merge from :idle, then :merge-pr → enters :merging-pr
      ;; → fires merge-pr-action
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:error :timeout})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses (task-responses {})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 704})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]
                (sc/send! session-id :wait-pr-merge)
                (sc/send! session-id :merge-pr)
                (resolvers/await-skill-threads!)
                (let [invs (claude-cli/invocations cli-nullable)
                      prompt (:prompt (:opts (first invs)))]
                  (is (= 1 (count invs)))
                  (is (= "/squash-merge-on-gh 704" prompt)))))))))

    (testing "execute-story-child-action includes task ID"
      ;; Story: send :has-tasks from :idle → enters :has-tasks
      ;; → fires execute-story-child-action
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:error :timeout})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses (story-responses
                                         {:meta {:refined "true"}}
                                         [{:status :open}])})]
          (mcp-tasks/with-nullable-mcp-tasks
            mcp-nullable
            (claude-cli/with-nullable-claude-cli
              cli-nullable
              (let [result
                    (graph/query
                     [`(resolvers/execute!
                        {:task/project-dir "/test" :task/id 705})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]
                (sc/send! session-id :has-tasks)
                (resolvers/await-skill-threads!)
                (let [invs (claude-cli/invocations cli-nullable)
                      prompt (:prompt (:opts (first invs)))]
                  (is (= 1 (count invs)))
                  (is
                   (= "/mcp-tasks:execute-story-child (MCP) 705" prompt)))))))))

    (testing "review-story-action includes task ID"
      ;; Story: send :done from :idle → enters :done → fires review-story-action
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:error :timeout})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses (story-responses
                                         {:meta {:refined "true"}}
                                         [{:status :closed}])})]
          (mcp-tasks/with-nullable-mcp-tasks
            mcp-nullable
            (claude-cli/with-nullable-claude-cli
              cli-nullable
              (let [result
                    (graph/query
                     [`(resolvers/execute!
                        {:task/project-dir "/test" :task/id 706})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]
                (sc/send! session-id :done)
                (resolvers/await-skill-threads!)
                (let [invs (claude-cli/invocations cli-nullable)
                      prompt (:prompt (:opts (first invs)))]
                  (is (= 1 (count invs)))
                  (is
                   (=
                    "/mcp-tasks:review-story-implementation (MCP) 706"
                    prompt)))))))))

    (testing "create-story-pr-action includes task ID"
      ;; Story: send :awaiting-pr from :idle → enters :awaiting-pr
      ;; → fires create-story-pr-action
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable {:error :timeout})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses (story-responses
                                         {:meta {:refined "true"}}
                                         [{:status :closed}])})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 707})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]
                (sc/send! session-id :awaiting-pr)
                (resolvers/await-skill-threads!)
                (let [invs (claude-cli/invocations cli-nullable)
                      prompt (:prompt (:opts (first invs)))]
                  (is (= 1 (count invs)))
                  (is
                   (= "/mcp-tasks:create-story-pr (MCP) 707" prompt)))))))))))

;;; Nullable Infrastructure Verification

(deftest nullable-tracks-invocations-test
  ;; Verify Nullable infrastructure tracks claude-cli invocations.
  (testing "Nullable infrastructure"
    (testing "tracks claude-cli invocations during skill execution"
      (with-integration-state
        (let [cli-nullable (claude-cli/make-nullable
                            {:exit-code 0 :events []})
              ;; initial execute! + re-derive after skill
              resps (merge-responses
                     (task-responses)
                     (task-responses
                      {:meta {:refined "true"}}))
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses resps})]

          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/my/project"
                                            :task/id 600})])
                    session-id (:execute/session-id
                                (get result `resolvers/execute!))]

                ;; Trigger skill invocation
                (sc/send! session-id :unrefined)
                (resolvers/await-skill-threads!)

                ;; Verify invocation was tracked with correct options
                (let [invs (claude-cli/invocations cli-nullable)
                      inv (first invs)
                      inv-opts (:opts inv)]
                  (is (= 1 (count invs)))
                  (is (= "/test" (:dir inv-opts)))
                  (is (= "/mcp-tasks:refine-task (MCP) 600" (:prompt inv-opts)))
                  (is (instance? java.time.Instant (:timestamp inv))))))))))))
