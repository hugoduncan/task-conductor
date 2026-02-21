(ns task-conductor.project.resolvers-test
  ;; Verify resolvers and mutations correctly delegate to registry.
  ;; Each test confirms the resolver/mutation calls the expected functions
  ;; and returns properly formatted results through Pathom.
  (:require
   [babashka.fs :as fs]
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

(defmacro with-clean-state
  "Execute body with clean registry and graph state."
  [& body]
  `(do
     (registry/clear!)
     (graph/reset-graph!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         (registry/clear!)
         (graph/reset-graph!)))))

;;; Resolver Tests

(deftest all-projects-test
  (with-clean-state
    (testing "all-projects"
      (testing "returns empty list when no projects registered"
        (let [result (graph/query [:project/all])]
          (is (= [] (:project/all result)))))

      (testing "returns all registered projects"
        (fs/with-temp-dir [tmp1]
          (fs/with-temp-dir [tmp2]
            (let [dir1 (str (fs/canonicalize tmp1))
                  dir2 (str (fs/canonicalize tmp2))]
              (registry/register! dir1 {:project/name "p1"})
              (registry/register! dir2 {:project/name "p2"})
              (let [result (graph/query [:project/all])
                    projects (:project/all result)
                    by-name (into
                             {}
                             (map (juxt :project/name identity))
                             projects)]
                (is (= 2 (count projects)))
                (is (= dir1 (:project/path (get by-name "p1"))))
                (is (= dir2 (:project/path (get by-name "p2"))))))))))))

(deftest project-by-path-test
  (with-clean-state
    (testing "project-by-path"
      (testing "returns project name for existing path"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))]
            (registry/register! dir {:project/name "myproj"})
            (let [result (graph/query {:project/path dir} [:project/name])]
              (is (= "myproj" (:project/name result)))))))

      (testing "returns empty map for non-existent path"
        (fs/with-temp-dir [tmp]
          (let [result (graph/query {:project/path (str tmp)} [:project/name])]
            (is (nil? (:project/name result)))))))))

(deftest project-by-name-test
  (with-clean-state
    (testing "project-by-name"
      (testing "returns project path for existing name"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))]
            (registry/register! dir {:project/name "myproj"})
            (let [result (graph/query {:project/name "myproj"} [:project/path])]
              (is (= dir (:project/path result)))))))

      (testing "returns empty map for non-existent name"
        (let [result (graph/query {:project/name "nosuch"} [:project/path])]
          (is (nil? (:project/path result))))))))

;;; Mutation Tests

(deftest project-create-test
  (with-clean-state
    (testing "project-create!"
      (testing "creates project with default name"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                result (graph/query [`(resolvers/project-create!
                                       {:project/path ~dir})])
                project (get-in
                         result
                         [`resolvers/project-create! :project/result])]
            (is (= dir (:project/path project)))
            (is (string? (:project/name project))))))

      (testing "creates project with custom name"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                result (graph/query [`(resolvers/project-create!
                                       {:project/path ~dir
                                        :project/name "custom"})])
                project (get-in
                         result
                         [`resolvers/project-create! :project/result])]
            (is (= dir (:project/path project)))
            (is (= "custom" (:project/name project))))))

      (testing "returns error for duplicate path"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))]
            (registry/register! dir)
            (let [result (graph/query [`(resolvers/project-create!
                                         {:project/path ~dir})])
                  error (get-in
                         result
                         [`resolvers/project-create! :project/result])]
              (is (= :duplicate-path (:error error)))))))

      (testing "returns error for non-existent path"
        (let [result (graph/query [`(resolvers/project-create!
                                     {:project/path "/no/such/path"})])
              error (get-in
                     result
                     [`resolvers/project-create! :project/result])]
          (is (= :path-not-found (:error error))))))))

(deftest project-update-test
  (with-clean-state
    (testing "project-update!"
      (testing "updates project name"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))]
            (registry/register! dir {:project/name "orig"})
            (let [result (graph/query [`(resolvers/project-update!
                                         {:project/path ~dir
                                          :project/name "renamed"})])
                  project (get-in
                           result
                           [`resolvers/project-update! :project/result])]
              (is (= "renamed" (:project/name project)))
              (is (= dir (:project/path project)))))))

      (testing "returns error for non-existent project"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                result (graph/query [`(resolvers/project-update!
                                       {:project/path ~dir
                                        :project/name "x"})])
                error (get-in
                       result
                       [`resolvers/project-update! :project/result])]
            (is (= :project-not-found (:error error))))))

      (testing "returns error for duplicate name"
        (fs/with-temp-dir [tmp1]
          (fs/with-temp-dir [tmp2]
            (let [dir1 (str (fs/canonicalize tmp1))
                  dir2 (str (fs/canonicalize tmp2))]
              (registry/register! dir1 {:project/name "taken"})
              (registry/register! dir2 {:project/name "other"})
              (let [result (graph/query [`(resolvers/project-update!
                                           {:project/path ~dir2
                                            :project/name "taken"})])
                    error (get-in
                           result
                           [`resolvers/project-update! :project/result])]
                (is (= :duplicate-name (:error error)))))))))))

(deftest project-delete-test
  (with-clean-state
    (testing "project-delete!"
      (testing "deletes project and returns it"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                project (registry/register! dir {:project/name "todel"})
                result (graph/query [`(resolvers/project-delete!
                                       {:project/path ~dir})])
                deleted (get-in
                         result
                         [`resolvers/project-delete! :project/result])]
            (is (= project deleted))
            (is (nil? (registry/get-by-path dir))))))

      (testing "returns nil for non-existent project"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                result (graph/query [`(resolvers/project-delete!
                                       {:project/path ~dir})])
                deleted (get-in
                         result
                         [`resolvers/project-delete! :project/result])]
            (is (nil? deleted))))))))

;;; Execute Test Helpers
;;; Builds responses in mcp-tasks CLI format (unnamespaced keys).
;;; The mcp-tasks resolvers add :task/ namespace via prefix-keys.

(defn make-task-response
  "Build a show-task CLI response map with sensible defaults.
   Returns {:task {...} :metadata {...}} matching CLI output format."
  ([] (make-task-response {}))
  ([overrides]
   {:task (merge {:type :task
                  :status :open
                  :meta nil
                  :pr-num nil
                  :code-reviewed nil}
                 overrides)
    :metadata {}}))

(defn make-blocking-response
  "Build a why-blocked CLI response map."
  ([] (make-blocking-response {}))
  ([overrides]
   (merge {:blocked-by []
           :blocking-reason nil}
          overrides)))

(defn make-children-response
  "Build a list-tasks CLI response for story children."
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
   Returns command-keyed map for mcp-tasks Nullable."
  ([] (task-responses {}))
  ([task-overrides]
   {:work-on [(make-work-on-response)]
    :show [(make-task-response task-overrides)]
    :why-blocked [(make-blocking-response)]}))

(defn story-responses
  "Build command-keyed responses for a story with children.
   Returns {:work-on [...] :show [...] :why-blocked [...] :list [...]}."
  [story-overrides children]
  {:work-on [(make-work-on-response)]
   :show [(make-task-response (merge {:type :story} story-overrides))]
   :why-blocked [(make-blocking-response)]
   :list [(make-children-response children)]})

(defmacro with-execute-state
  "Execute body with clean state for execute! tests.
   Resets engine, graph, registries, and registers required resolvers
   including dev-env resolvers for proper dev-env selection."
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
     ;; Register all resolvers
     (engine-resolvers/register-resolvers!)
     (dev-env-resolvers/register-resolvers!)
     (mcp-tasks-resolvers/register-resolvers!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         ;; Wait for all skill threads to complete before cleanup
         (resolvers/await-skill-threads!)
         (resolvers/reset-skill-threads!)
         (engine/reset-engine!)
         (graph/reset-graph!)
         (registry/clear!)
         (dev-env-registry/clear!)
         (engine-resolvers/reset-dev-env-hooks!)))))

;;; Execute Mutation Tests

(deftest execute-test
  ;; Verify execute! mutation correctly starts statechart sessions for tasks
  ;; and stories, and derives initial state.
  (testing "execute!"
    (testing "starts statechart session for a task"
      (with-execute-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses (task-responses
                                     {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 123})])
                    execute-result (get result `resolvers/execute!)]
                (is (string? (:execute/session-id execute-result)))
                (is (= :refined (:execute/initial-state execute-result)))
                (is (nil? (:execute/error execute-result)))))))))

    (testing "uses task-statechart for non-story types"
      (with-execute-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses (task-responses {:type :bug})})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 456})])
                    execute-result (get result `resolvers/execute!)
                    session-id (:execute/session-id execute-result)]
                (is (= :unrefined (:execute/initial-state execute-result)))
                ;; Verify session is using task-statechart (starts in :idle)
                (is (contains? (sc/current-state session-id) :idle))))))))

    (testing "starts statechart session for a story"
      (with-execute-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses (story-responses
                                     {:meta {:refined "true"}}
                                     [{:status :open} {:status :closed}])})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 789})])
                    execute-result (get result `resolvers/execute!)]
                (is (string? (:execute/session-id execute-result)))
                ;; Has incomplete children, so state is :has-tasks
                (is
                 (= :has-tasks (:execute/initial-state execute-result)))))))))

    (testing "derives :done for story with all children complete"
      (with-execute-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses (story-responses
                                     {:meta {:refined "true"}}
                                     [{:status :closed} {:status :closed}])})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 999})])
                    execute-result (get result `resolvers/execute!)]
                (is (= :done (:execute/initial-state execute-result)))))))))

    (testing "returns error when task not found"
      (with-execute-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses {:work-on [(make-work-on-response)]
                                     :show [{:error :not-found
                                             :message "Not found"}]}})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 404})])
                    execute-result (get result `resolvers/execute!)]
                (is (= :not-found (:error (:execute/error execute-result))))
                (is (nil? (:execute/session-id execute-result)))))))))

    (testing "does not register dev-env hooks on execute"
      ;; Idle detection is handled by CLI hooks in escalate-to-dev-env!,
      ;; not at execute! time.
      (with-execute-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              _dev-env-id (dev-env-registry/register! dev-env :test)
              nullable (mcp-tasks/make-nullable
                        {:responses (task-responses
                                     {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 123})])
                    execute-result (get result `resolvers/execute!)
                    session-id (:execute/session-id execute-result)]
                (is (string? session-id))
                (is (= 0 (count @(:hooks dev-env))))))))))

    (testing "stores session data including task context"
      (with-execute-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses (story-responses
                                     {:meta {:refined "true"}}
                                     [{:status :open}])})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/my/project"
                                            :task/id 555
                                            :task/nrepl-port "7888"})])
                    execute-result (get result `resolvers/execute!)
                    session-id (:execute/session-id execute-result)
                    data (sc/get-data session-id)]
                (is (= "/test" (:project-dir data)))
                (is (= 555 (:task-id data)))
                (is (= :story (:task-type data)))
                (is (= session-id (:session-id data)))
                (is (= "7888" (:nrepl-port data)))))))))

    (testing "omits nrepl-port from session data when not provided"
      (with-execute-state
        (let [nullable (mcp-tasks/make-nullable
                        {:responses (task-responses
                                     {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [result (graph/query [`(resolvers/execute!
                                           {:task/project-dir "/test"
                                            :task/id 556})])
                    execute-result (get result `resolvers/execute!)
                    session-id (:execute/session-id execute-result)
                    data (sc/get-data session-id)]
                (is (not (contains? data :nrepl-port)))))))))))

;;; Invoke Skill Tests

(deftest invoke-skill-test
  ;; Verify invoke-skill! starts claude-cli and returns immediately.
  ;; Uses claude-cli Nullable to track invocations.
  (testing "invoke-skill!"
    (testing "invokes claude-cli with skill prompt"
      (with-execute-state
        (let [cli-nullable (claude-cli/make-nullable {:exit-code 0 :events []})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             (task-responses {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              ;; Start an execute session to get valid session-id with data
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test/dir"
                                                 :task/id 99})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    ;; Now invoke skill
                    result (graph/query [`(resolvers/invoke-skill!
                                           {:skill "mcp-tasks:refine-task (MCP)"
                                            :engine/session-id ~session-id})])
                    invoke-result (get result `resolvers/invoke-skill!)]
                (is (= :started (:invoke-skill/status invoke-result)))
                ;; Wait for skill thread to complete
                (resolvers/await-skill-threads!)
                ;; Verify invocation was tracked
                (let [invs (claude-cli/invocations cli-nullable)]
                  (is (= 1 (count invs)))
                  (is
                   (=
                    "/mcp-tasks:refine-task (MCP)"
                    (:prompt (:opts (first invs)))))
                  (is (= "/test" (:dir (:opts (first invs))))))))))))

    (testing "transitions to :escalated on skill error"
      (with-execute-state
        (let [cli-nullable (claude-cli/make-nullable {:error :timeout})
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             (task-responses {:meta {:refined "true"}})})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 100})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    ;; Trigger skill via state event
                    _ (sc/send! session-id :refined)]
                ;; Wait for skill thread to process error
                (resolvers/await-skill-threads!)
                ;; Verify transitioned to :escalated state
                (is
                 (contains? (sc/current-state session-id) :escalated))))))))))

(testing "transitions to :escalated on non-zero exit code"
  (with-execute-state
    (let [cli-nullable (claude-cli/make-nullable {:exit-code 1})
          mcp-nullable (mcp-tasks/make-nullable
                        {:responses
                         (task-responses {:meta {:refined "true"}})})
          dev-env (dev-env-protocol/make-noop-dev-env)
          _ (dev-env-registry/register! dev-env :test)]
      (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
        (claude-cli/with-nullable-claude-cli cli-nullable
          (let [work-result (graph/query [`(resolvers/execute!
                                            {:task/project-dir "/test"
                                             :task/id 103})])
                session-id (:execute/session-id
                            (get work-result `resolvers/execute!))
                _ (sc/send! session-id :refined)]
            (resolvers/await-skill-threads!)
            (is
             (contains? (sc/current-state session-id) :escalated)
             "non-zero exit code triggers escalation")))))))

;;; Concurrent Skill Invocation Tests

(deftest concurrent-skill-invocation-test
  ;; Verify multiple concurrent skill invocations are handled correctly.
  ;; Tests thread safety via claude-cli Nullable invocation tracking.
  (testing "concurrent skill invocations"
    (testing "handles multiple skills invoked in parallel"
      (with-execute-state
        (let [cli-nullable (claude-cli/make-nullable
                            {:exit-code 0 :events []})
              ;; Enough responses for skills + transitions
              show-resps (vec (repeat 10
                                      (make-task-response
                                       {:meta {:refined "true"}})))
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             {:work-on [(make-work-on-response)]
                              :show show-resps
                              :why-blocked
                              [(make-blocking-response)]}})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              ;; Start an execute session
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 200})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))]
                ;; Invoke multiple skills concurrently
                (dotimes [i 3]
                  (graph/query [`(resolvers/invoke-skill!
                                  {:skill ~(str "skill-" i)
                                   :engine/session-id ~session-id})]))
                ;; Wait for skill threads
                (resolvers/await-skill-threads!)
                ;; All three manually invoked skills should be present
                (let [invs (claude-cli/invocations cli-nullable)
                      prompts (set (map #(:prompt (:opts %)) invs))]
                  (is (>= (count invs) 3) "At least 3 skills should be invoked")
                  (is (contains? prompts "/skill-0"))
                  (is (contains? prompts "/skill-1"))
                  (is (contains? prompts "/skill-2")))))))))

    (testing "each concurrent invocation gets correct session data"
      (with-execute-state
        (let [cli-nullable (claude-cli/make-nullable
                            {:exit-code 0 :events []})
              ;; Enough responses for execute!,
              ;; store-pre-skill-state!, on-skill-complete
              show-resps
              (vec (repeat 10
                           (make-task-response
                            {:meta {:refined "true"}})))
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses
                {:work-on
                 [(make-work-on-response
                   {:worktree-path "/project-a"})
                  (make-work-on-response
                   {:worktree-path "/project-b"})]
                 :show show-resps
                 :why-blocked
                 [(make-blocking-response)
                  (make-blocking-response)]}})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              ;; Start two sessions with different project dirs
              (let [result1 (graph/query [`(resolvers/execute!
                                            {:task/project-dir "/project-a"
                                             :task/id 301})])
                    session1 (:execute/session-id
                              (get result1 `resolvers/execute!))
                    result2 (graph/query [`(resolvers/execute!
                                            {:task/project-dir "/project-b"
                                             :task/id 302})])
                    session2 (:execute/session-id
                              (get result2 `resolvers/execute!))]
                ;; Invoke skills on both sessions
                (graph/query [`(resolvers/invoke-skill!
                                {:skill "test"
                                 :engine/session-id ~session1})])
                (graph/query [`(resolvers/invoke-skill!
                                {:skill "test"
                                 :engine/session-id ~session2})])
                ;; Wait for skill threads
                (resolvers/await-skill-threads!)
                ;; Verify each got the right project dir
                (let [invs (claude-cli/invocations cli-nullable)
                      dirs (set (map #(:dir (:opts %)) invs))]
                  (is (contains? dirs "/project-a"))
                  (is (contains? dirs "/project-b")))))))))))

;;; No-Progress Detection Tests

(deftest no-progress-detection-test
  ;; Verify skill completion detects no-progress when state doesn't change
  ;; and escalates to dev-env with Claude session-id.
  (testing "no-progress detection"
    (testing "sends :no-progress when re-derived state matches current state"
      (with-execute-state
        ;; Configure CLI to return a session-id for resumption
        (let [cli-nullable
              (claude-cli/make-nullable
               {:exit-code 0 :events []
                :session-id "claude-abc"})
              ;; Task stays :refined after skill
              refined-resp (make-task-response
                            {:meta {:refined "true"}})
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses
                {:work-on [(make-work-on-response)]
                 :show (repeat 3 refined-resp)
                 :why-blocked
                 [(make-blocking-response)]}})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 100})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    _ (sc/send! session-id :refined)]
                ;; Wait for skill to complete
                (resolvers/await-skill-threads!)
                ;; Should have transitioned to :escalated via :no-progress
                (is (contains? (sc/current-state session-id) :escalated))
                ;; Verify Claude session-id was stored
                (let [data (sc/get-data session-id)]
                  (is (= "claude-abc" (:last-claude-session-id data))))))))))

    (testing "sends state event when progress is made"
      (with-execute-state
        (let [cli-nullable (claude-cli/make-nullable
                            {:exit-code 0 :events []})
              ;; Task moves from :refined to :wait-pr-merge
              ;; Third show response has pr-num set
              refined-resp (make-task-response
                            {:meta {:refined "true"}})
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses
                {:work-on [(make-work-on-response)]
                 :show [refined-resp
                        refined-resp
                        (make-task-response
                         {:meta {:refined "true"}
                          :pr-num 42})]
                 :why-blocked
                 [(make-blocking-response)]}})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 101})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    _ (sc/send! session-id :refined)]
                (resolvers/await-skill-threads!)
                ;; Should have transitioned to :wait-pr-merge (progress made)
                ;; pr-num set means derive-task-state returns :wait-pr-merge
                (let [hist (sc/history session-id)
                      events (mapv :event hist)]
                  (is (some #{:wait-pr-merge} events)
                      "Should have received :wait-pr-merge event"))))))))

    (testing "sends :no-progress for :has-tasks when open children unchanged"
      (with-execute-state
        (let [cli-nullable
              (claude-cli/make-nullable
               {:exit-code 0 :events []
                :session-id "claude-def"})
              ;; 2 open children before and after skill
              story-resp
              (make-task-response
               {:type :story
                :meta {:refined "true"}})
              two-open
              (make-children-response
               [{:status :open} {:status :open}])
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses
                {:work-on [(make-work-on-response)]
                 :show (repeat 3 story-resp)
                 :why-blocked
                 [(make-blocking-response)]
                 :list (repeat 3 two-open)}})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 102})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    _ (sc/send! session-id :has-tasks)]
                (resolvers/await-skill-threads!)
                ;; Should have transitioned to :escalated (no progress)
                (is (contains? (sc/current-state session-id) :escalated))
                (is
                 (=
                  "claude-def"
                  (:last-claude-session-id (sc/get-data session-id))))))))))

    (testing "sends :has-tasks event when open children count decreases"
      (with-execute-state
        (let [cli-nullable (claude-cli/make-nullable
                            {:exit-code 0 :events []})
              ;; 2 open before, 1 after skill
              story-resp
              (make-task-response
               {:type :story
                :meta {:refined "true"}})
              two-open
              (make-children-response
               [{:status :open} {:status :open}])
              one-open
              (make-children-response
               [{:status :open} {:status :closed}])
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses
                {:work-on [(make-work-on-response)]
                 :show (repeat 3 story-resp)
                 :why-blocked
                 [(make-blocking-response)]
                 :list [two-open two-open one-open]}})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 103})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    _ (sc/send! session-id :has-tasks)]
                (resolvers/await-skill-threads!)
                ;; Should have received :has-tasks event (progress made)
                (let [hist (sc/history session-id)
                      events (mapv :event hist)]
                  (is (some #{:has-tasks} (rest events))
                      "Should have :has-tasks event"))))))))))

;;; Escalate to Dev-env Tests

(deftest escalate-to-dev-env-test
  ;; Verify escalate-to-dev-env! starts a dev-env session.
  (testing "escalate-to-dev-env!"
    (testing "passes claude-session-id to dev-env when available"
      (with-execute-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              dev-env-id (dev-env-registry/register! dev-env :test)
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             (task-responses {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 300})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    ;; Store Claude session-id (simulates no-progress)
                    _ (sc/update-data!
                       session-id
                       #(assoc % :last-claude-session-id "claude-xyz"))
                    result (graph/query [`(resolvers/escalate-to-dev-env!
                                           {:engine/session-id ~session-id})])
                    escalate-result (get result `resolvers/escalate-to-dev-env!)
                    calls @(:calls dev-env)
                    start-call (first
                                (filter #(= :start-session (:op %)) calls))]
                (is (= :escalated (:escalate/status escalate-result)))
                (is (= dev-env-id (:escalate/dev-env-id escalate-result)))
                (is (some? start-call))
                ;; Verify Claude session-id was passed in opts
                (is
                 (=
                  "claude-xyz"
                  (get-in start-call [:opts :claude-session-id])))))))))

    (testing "starts dev-env session when available"
      (with-execute-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              dev-env-id (dev-env-registry/register! dev-env :test)
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             (task-responses {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 200})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    result (graph/query [`(resolvers/escalate-to-dev-env!
                                           {:engine/session-id ~session-id})])
                    escalate-result (get result `resolvers/escalate-to-dev-env!)
                    ;; Check that start-session was called via the :calls atom
                    calls @(:calls dev-env)
                    start-call (first
                                (filter #(= :start-session (:op %)) calls))]
                (is (= :escalated (:escalate/status escalate-result)))
                (is (= dev-env-id (:escalate/dev-env-id escalate-result)))
                (is (some? start-call) "start-session should have been called")
                (is (= session-id (:session-id start-call)))))))))

    (testing "registers on-close hook with dev-env"
      (with-execute-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             (task-responses {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 350})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    result (graph/query [`(resolvers/escalate-to-dev-env!
                                           {:engine/session-id ~session-id})])
                    escalate-result (get result `resolvers/escalate-to-dev-env!)
                    calls @(:calls dev-env)
                    on-close-calls (filter #(and (= :register-hook (:op %))
                                                 (= :on-close (:hook-type %)))
                                           calls)]
                (is (= :escalated (:escalate/status escalate-result)))
                (is (= 1 (count on-close-calls))
                    "should register exactly one on-close hook")
                (is (= session-id (:session-id (first on-close-calls))))))))))

    (testing "on-close hook re-derives state and sends event to statechart"
      (with-execute-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)
              ;; Responses consumed by:
              ;; 1. execute! fetch-task (refined)
              ;; 2. store-pre-skill-state! from :refined entry action (refined)
              ;; 3. on-skill-complete virtual thread (refined → no-progress)
              ;; 4. on-dev-env-close re-derive (done)
              refined-resp
              (make-task-response
               {:meta {:refined "true"}})
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses
                {:work-on [(make-work-on-response)]
                 :show [refined-resp
                        refined-resp
                        refined-resp
                        (make-task-response
                         {:status :done
                          :meta {:refined "true"}})]
                 :why-blocked
                 [(make-blocking-response)]}})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 351})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))]
                ;; Enter :refined (triggers invoke-skill!),
                ;; then send :error to transition to :escalated
                (sc/send! session-id :refined)
                (sc/send! session-id :error)
                (is (contains? (sc/current-state session-id) :escalated))
                ;; Wait for skill thread from :refined entry action to complete
                (resolvers/await-skill-threads!)
                (is (contains? (sc/current-state session-id) :escalated))
                ;; Find and invoke the on-close callback
                (let [hooks @(:hooks dev-env)
                      on-close-hook (first
                                     (filter
                                      #(= :on-close (:type (val %)))
                                      hooks))
                      callback (:callback (val on-close-hook))]
                  ;; Simulate buffer close
                  (callback {:session-id session-id
                             :timestamp (java.time.Instant/now)
                             :reason :user-exit})
                  ;; Should have transitioned from :escalated to :done
                  (is (contains? (sc/current-state session-id) :done)))))))))

    (testing "passes CLI hooks from session-stored nrepl-port"
      (with-execute-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses (task-responses {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result
                    (graph/query [`(resolvers/execute!
                                    {:task/project-dir "/test"
                                     :task/id 300
                                     :task/nrepl-port "7888"})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    _ (graph/query [`(resolvers/escalate-to-dev-env!
                                      {:engine/session-id ~session-id})])
                    calls @(:calls dev-env)
                    start-call (first
                                (filter #(= :start-session (:op %))
                                        calls))
                    hooks (get-in start-call [:opts :hooks])]
                (is (some? hooks) "hooks should be present in opts")
                (is (contains? hooks :UserPromptSubmit))
                (is (contains? hooks :Notification))
                ;; Verify hook commands contain session-id and port
                (let [active-cmd (get-in hooks
                                         [:UserPromptSubmit 0
                                          :hooks 0 :command])
                      idle-cmd (get-in hooks
                                       [:Notification 0
                                        :hooks 0 :command])]
                  (is (re-find (re-pattern (str session-id))
                               active-cmd))
                  (is (re-find #"7888" active-cmd))
                  (is (re-find #":on-active" active-cmd))
                  (is (re-find (re-pattern (str session-id))
                               idle-cmd))
                  (is (re-find #"7888" idle-cmd))
                  (is (re-find #":on-session-idle" idle-cmd)))))))))

    (testing "falls back to .nrepl-port file when session has no port"
      (fs/with-temp-dir [tmp-dir]
        (let [project-dir (str (fs/canonicalize tmp-dir))]
          (spit (str project-dir "/.nrepl-port") "9999")
          (with-execute-state
            (let [dev-env (dev-env-protocol/make-noop-dev-env)
                  _ (dev-env-registry/register! dev-env :test)
                  mcp-nullable
                  (mcp-tasks/make-nullable
                   {:responses
                    {:work-on [(make-work-on-response
                                {:worktree-path project-dir})]
                     :show [(make-task-response {:meta {:refined "true"}})]
                     :why-blocked [(make-blocking-response)]}})]
              (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
                (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
                  (let [work-result
                        (graph/query [`(resolvers/execute!
                                        {:task/project-dir ~project-dir
                                         :task/id 300})])
                        session-id (:execute/session-id
                                    (get work-result `resolvers/execute!))
                        _ (graph/query [`(resolvers/escalate-to-dev-env!
                                          {:engine/session-id ~session-id})])
                        calls @(:calls dev-env)
                        start-call (first
                                    (filter #(= :start-session (:op %))
                                            calls))
                        hooks (get-in start-call [:opts :hooks])]
                    (is (some? hooks) "hooks from .nrepl-port fallback")
                    (let [active-cmd (get-in hooks
                                             [:UserPromptSubmit 0
                                              :hooks 0 :command])]
                      (is (re-find #"9999" active-cmd)))))))))))

    (testing "omits hooks when no nrepl-port available"
      (with-execute-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)
              mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             (task-responses {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result
                    (graph/query [`(resolvers/execute!
                                    {:task/project-dir "/test"
                                     :task/id 300})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    _ (graph/query [`(resolvers/escalate-to-dev-env!
                                      {:engine/session-id ~session-id})])
                    calls @(:calls dev-env)
                    start-call (first
                                (filter #(= :start-session (:op %))
                                        calls))]
                (is (nil? (get-in start-call [:opts :hooks]))
                    "hooks absent when no nrepl-port")))))))

    (testing "returns error when no dev-env available"
      (with-execute-state
        (let [mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             (task-responses {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 201})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    result (graph/query [`(resolvers/escalate-to-dev-env!
                                           {:engine/session-id ~session-id})])
                    escalate-result (get
                                     result
                                     `resolvers/escalate-to-dev-env!)]
                (is (= :no-dev-env (:escalate/status escalate-result)))
                (is
                 (=
                  :no-dev-env
                  (:error (:escalate/error escalate-result))))))))))))

;;; Hook Command Roundtrip Tests

(defn- extract-event-keyword
  "Parse the statechart event keyword from a hook command string.
  The command contains a send! call like:
    (task-conductor.statechart-engine.interface/send! \"sid\" :on-active)
  Returns the keyword (e.g. :on-active)."
  [command-str]
  (when-let [m (re-find #"(:[a-z-]+)\)" command-str)]
    (keyword (subs (second m) 1))))

(deftest hook-command-statechart-roundtrip-test
  ;; Verify the full path from hook command generation to statechart
  ;; sub-state transition. Bridges the gap between build-session-hooks
  ;; (string generation) and statechart behavior (event handling).
  (testing "hook command -> statechart event roundtrip"
    (testing "generated hook commands trigger correct sub-state transitions"
      (with-execute-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses
                {:work-on [(make-work-on-response)]
                 :show [(make-task-response)]
                 :why-blocked [(make-blocking-response)]}})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result
                    (graph/query [`(resolvers/execute!
                                    {:task/project-dir "/test"
                                     :task/id 600
                                     :task/nrepl-port "7888"})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    ;; Transition idle -> unrefined -> escalated
                    _ (sc/send! session-id :unrefined)
                    _ (resolvers/await-skill-threads!)
                    ;; Send :error to transition to :escalated
                    ;; Entry action calls escalate-to-dev-env! which
                    ;; reads nrepl-port from session data and passes hooks
                    _ (sc/send! session-id :error)
                    _ (is (= #{:escalated :session-idle}
                             (sc/current-state session-id)))
                    calls @(:calls dev-env)
                    start-call (first
                                (filter #(= :start-session (:op %))
                                        calls))
                    hooks (get-in start-call [:opts :hooks])
                    ;; Extract event keywords from command strings
                    active-cmd (get-in hooks
                                       [:UserPromptSubmit 0
                                        :hooks 0 :command])
                    idle-cmd (get-in hooks
                                     [:Notification 0
                                      :hooks 0 :command])
                    active-event (extract-event-keyword active-cmd)
                    idle-event (extract-event-keyword idle-cmd)]
                ;; Verify parsing extracted the expected events
                (is (= :on-active active-event)
                    "UserPromptSubmit hook should send :on-active")
                (is (= :on-session-idle idle-event)
                    "Notification hook should send :on-session-idle")
                ;; Send the active event extracted from hook command
                (sc/send! session-id active-event)
                (is (= #{:escalated :session-running}
                       (sc/current-state session-id))
                    "active hook event transitions to :session-running")
                ;; Send the idle event extracted from hook command
                (sc/send! session-id idle-event)
                (is (= #{:escalated :session-idle}
                       (sc/current-state session-id))
                    "idle hook event transitions back to :session-idle")
                ;; Verify commands contain the session-id for routing
                (is (re-find (re-pattern (str session-id)) active-cmd)
                    "active command routes to correct session")
                (is (re-find (re-pattern (str session-id)) idle-cmd)
                    "idle command routes to correct session")))))))

    (testing "hook commands contain valid send! form structure"
      (with-execute-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses (task-responses {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result
                    (graph/query [`(resolvers/execute!
                                    {:task/project-dir "/test"
                                     :task/id 601
                                     :task/nrepl-port "9999"})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    _ (graph/query [`(resolvers/escalate-to-dev-env!
                                      {:engine/session-id ~session-id})])
                    calls @(:calls dev-env)
                    start-call (first
                                (filter #(= :start-session (:op %))
                                        calls))
                    hooks (get-in start-call [:opts :hooks])
                    active-cmd (get-in hooks
                                       [:UserPromptSubmit 0
                                        :hooks 0 :command])
                    idle-cmd (get-in hooks
                                     [:Notification 0
                                      :hooks 0 :command])]
                ;; Verify heredoc structure
                (is (re-find #"read -r -d '' CODE << 'EoC'" active-cmd)
                    "uses heredoc quoting")
                (is (re-find #"EoC\n" active-cmd)
                    "heredoc terminator present")
                ;; Verify send! targets correct namespace
                (is (re-find
                     #"statechart-engine\.interface/send!"
                     active-cmd)
                    "calls statechart engine interface")
                ;; Verify clj-nrepl-eval with correct port
                (is (re-find #"clj-nrepl-eval -p 9999" active-cmd)
                    "uses correct nrepl port")
                (is (re-find #"clj-nrepl-eval -p 9999" idle-cmd)
                    "idle command uses correct nrepl port")))))))))

;;; PR Merge Tests

(deftest pr-merge-test
  ;; Verify pr-merge! sends :merge-pr event to statechart when session
  ;; is in :wait-pr-merge state, and rejects when in wrong state.
  (testing "pr-merge!"
    (testing "sends :merge-pr event when session is in :wait-pr-merge"
      (with-execute-state
        (let [cli-nullable (claude-cli/make-nullable {:exit-code 0 :events []})
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses
                {:work-on [(make-work-on-response)]
                 :show [(make-task-response {:meta {:refined "true"}
                                             :pr-num 42})
                        (make-task-response {:meta {:refined "true"}
                                             :pr-num 42})
                        (make-task-response {:meta {:refined "true"}
                                             :pr-num 42
                                             :status :closed})]
                 :why-blocked [(make-blocking-response)]}})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 500})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    ;; Transition to :wait-pr-merge
                    _ (sc/send! session-id :wait-pr-merge)
                    _ (is (contains? (sc/current-state session-id)
                                     :wait-pr-merge)
                          "precondition: session in :wait-pr-merge")
                    ;; Invoke pr-merge!
                    result (graph/query [`(resolvers/pr-merge!
                                           {:engine/session-id ~session-id})])
                    merge-result (get result `resolvers/pr-merge!)]
                (is (= :triggered (:pr-merge/status merge-result)))
                (is (nil? (:pr-merge/error merge-result)))
                ;; Should have transitioned to :merging-pr
                ;; (skill thread cleanup handled by with-execute-state finally)
                (is (contains? (sc/current-state session-id) :merging-pr))))))))

    (testing "completes when merge skill succeeds regardless of API state"
      ;; merge-pr-action sets :on-complete :complete, so a successful
      ;; merge skill sends :complete directly without re-derivation.
      (with-execute-state
        (let [cli-nullable (claude-cli/make-nullable {:exit-code 0 :events []})
              mcp-nullable
              (mcp-tasks/make-nullable
               {:responses
                {:work-on [(make-work-on-response)]
                 :show [(make-task-response {:meta {:refined "true"}
                                             :pr-num 42})
                        (make-task-response {:meta {:refined "true"}
                                             :pr-num 42})]
                 :why-blocked [(make-blocking-response)]}})
              dev-env (dev-env-protocol/make-noop-dev-env)
              _ (dev-env-registry/register! dev-env :test)]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli cli-nullable
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 502})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    _ (sc/send! session-id :wait-pr-merge)
                    _ (graph/query [`(resolvers/pr-merge!
                                      {:engine/session-id ~session-id})])]
                (resolvers/await-skill-threads!)
                (is (= #{} (sc/current-state session-id))
                    "completes when merge succeeds")))))))

    (testing "returns error when session not in :wait-pr-merge"
      (with-execute-state
        (let [mcp-nullable (mcp-tasks/make-nullable
                            {:responses
                             (task-responses {:meta {:refined "true"}})})]
          (mcp-tasks/with-nullable-mcp-tasks mcp-nullable
            (claude-cli/with-nullable-claude-cli (claude-cli/make-nullable)
              (let [work-result (graph/query [`(resolvers/execute!
                                                {:task/project-dir "/test"
                                                 :task/id 501})])
                    session-id (:execute/session-id
                                (get work-result `resolvers/execute!))
                    ;; Session starts in :idle, not :wait-pr-merge
                    result (graph/query [`(resolvers/pr-merge!
                                           {:engine/session-id ~session-id})])
                    merge-result (get result `resolvers/pr-merge!)]
                (is (= :error (:pr-merge/status merge-result)))
                (is (= :invalid-state
                       (:error (:pr-merge/error merge-result))))))))))

    (testing "returns error when session not found"
      (with-execute-state
        (let [result (graph/query [`(resolvers/pr-merge!
                                     {:engine/session-id "no-such-id"})])
              merge-result (get result `resolvers/pr-merge!)]
          (is (= :error (:pr-merge/status merge-result)))
          (is (= :session-not-found
                 (:error (:pr-merge/error merge-result)))))))))
