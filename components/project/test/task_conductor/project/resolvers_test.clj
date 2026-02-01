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
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.registry :as registry]
   [task-conductor.project.resolvers :as resolvers]
   [task-conductor.project.work-on :as work-on]
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
                    by-name (into {} (map (juxt :project/name identity)) projects)]
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
                project (get-in result [`resolvers/project-create! :project/result])]
            (is (= dir (:project/path project)))
            (is (string? (:project/name project))))))

      (testing "creates project with custom name"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                result (graph/query [`(resolvers/project-create!
                                       {:project/path ~dir
                                        :project/name "custom"})])
                project (get-in result [`resolvers/project-create! :project/result])]
            (is (= dir (:project/path project)))
            (is (= "custom" (:project/name project))))))

      (testing "returns error for duplicate path"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))]
            (registry/register! dir)
            (let [result (graph/query [`(resolvers/project-create!
                                         {:project/path ~dir})])
                  error (get-in result [`resolvers/project-create! :project/result])]
              (is (= :duplicate-path (:error error)))))))

      (testing "returns error for non-existent path"
        (let [result (graph/query [`(resolvers/project-create!
                                     {:project/path "/no/such/path"})])
              error (get-in result [`resolvers/project-create! :project/result])]
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
                  project (get-in result [`resolvers/project-update! :project/result])]
              (is (= "renamed" (:project/name project)))
              (is (= dir (:project/path project)))))))

      (testing "returns error for non-existent project"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                result (graph/query [`(resolvers/project-update!
                                       {:project/path ~dir
                                        :project/name "x"})])
                error (get-in result [`resolvers/project-update! :project/result])]
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
                    error (get-in result [`resolvers/project-update! :project/result])]
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
                deleted (get-in result [`resolvers/project-delete! :project/result])]
            (is (= project deleted))
            (is (nil? (registry/get-by-path dir))))))

      (testing "returns nil for non-existent project"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                result (graph/query [`(resolvers/project-delete!
                                       {:project/path ~dir})])
                deleted (get-in result [`resolvers/project-delete! :project/result])]
            (is (nil? deleted))))))))

;;; Work-on Test Helpers

(defn make-task
  "Create task data with sensible defaults.
   Accepts overrides for :type, :status, :meta, :pr-num, :code-reviewed, :error."
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

(defmacro with-work-on-state
  "Execute body with clean state for work-on! tests.
   Resets engine, graph, registries, and registers required resolvers
   including dev-env resolvers for proper dev-env selection."
  [& body]
  `(do
     (engine/reset-engine!)
     (graph/reset-graph!)
     (registry/clear!)
     (dev-env-registry/clear!)
     (engine-resolvers/reset-dev-env-hooks!)
     ;; Register statecharts
     (work-on/register-statecharts!)
     ;; Register all resolvers
     (engine-resolvers/register-resolvers!)
     (dev-env-resolvers/register-resolvers!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         ;; Allow virtual threads to complete before cleanup
         (Thread/sleep 50)
         (engine/reset-engine!)
         (graph/reset-graph!)
         (registry/clear!)
         (dev-env-registry/clear!)
         (engine-resolvers/reset-dev-env-hooks!)))))

(defmacro with-mock-fetchers
  "Execute body with mocked fetch-task and fetch-children.
   task-fn: (fn [dir id] task-map)
   children-fn: (fn [dir parent-id] [child-maps])"
  [task-fn children-fn & body]
  `(with-redefs [resolvers/fetch-task ~task-fn
                 resolvers/fetch-children ~children-fn]
     ~@body))

;;; Work-on Mutation Tests

(deftest work-on-test
  ;; Verify work-on! mutation correctly starts statechart sessions for tasks
  ;; and stories, derives initial state, and registers dev-env hooks.
  (testing "work-on!"
    (testing "starts statechart session for a task"
      (with-work-on-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
          (fn [_dir _id] [])
          (let [result (graph/query [`(resolvers/work-on!
                                       {:task/project-dir "/test"
                                        :task/id 123})])
                work-on-result (get result `resolvers/work-on!)]
            (is (string? (:work-on/session-id work-on-result)))
            (is (= :refined (:work-on/initial-state work-on-result)))
            (is (nil? (:work-on/error work-on-result)))))))

    (testing "uses task-statechart for non-story types"
      (with-work-on-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/type :bug}))
          (fn [_dir _id] [])
          (let [result (graph/query [`(resolvers/work-on!
                                       {:task/project-dir "/test"
                                        :task/id 456})])
                work-on-result (get result `resolvers/work-on!)
                session-id (:work-on/session-id work-on-result)]
            (is (= :unrefined (:work-on/initial-state work-on-result)))
            ;; Verify session is using task-statechart (starts in :idle)
            (is (contains? (sc/current-state session-id) :idle))))))

    (testing "starts statechart session for a story"
      (with-work-on-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/type :story
                                     :task/meta {:refined "true"}}))
          (fn [_dir _id] [(make-child) (make-child {:task/status :closed})])
          (let [result (graph/query [`(resolvers/work-on!
                                       {:task/project-dir "/test"
                                        :task/id 789})])
                work-on-result (get result `resolvers/work-on!)]
            (is (string? (:work-on/session-id work-on-result)))
            ;; Has incomplete children, so state is :has-tasks
            (is (= :has-tasks (:work-on/initial-state work-on-result)))))))

    (testing "derives :awaiting-review for story with all children complete"
      (with-work-on-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/type :story
                                     :task/meta {:refined "true"}}))
          (fn [_dir _id] [(make-child {:task/status :closed})
                          (make-child {:task/status :closed})])
          (let [result (graph/query [`(resolvers/work-on!
                                       {:task/project-dir "/test"
                                        :task/id 999})])
                work-on-result (get result `resolvers/work-on!)]
            (is (= :awaiting-review (:work-on/initial-state work-on-result)))))))

    (testing "returns error when task not found"
      (with-work-on-state
        (with-mock-fetchers
          (fn [_dir _id] {:task/error {:error :not-found :message "Not found"}})
          (fn [_dir _id] [])
          (let [result (graph/query [`(resolvers/work-on!
                                       {:task/project-dir "/test"
                                        :task/id 404})])
                work-on-result (get result `resolvers/work-on!)]
            (is (= :not-found (:error (:work-on/error work-on-result))))
            (is (nil? (:work-on/session-id work-on-result)))))))

    (testing "registers dev-env hook when dev-env available"
      (with-work-on-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              _dev-env-id (dev-env-registry/register! dev-env :test)]
          (with-mock-fetchers
            (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
            (fn [_dir _id] [])
            (let [result (graph/query [`(resolvers/work-on!
                                         {:task/project-dir "/test"
                                          :task/id 123})])
                  work-on-result (get result `resolvers/work-on!)
                  session-id (:work-on/session-id work-on-result)]
              (is (string? session-id))
              ;; Verify hook was registered by checking the dev-env's hooks atom
              (is (= 1 (count @(:hooks dev-env)))))))))

    (testing "stores session data including task context"
      (with-work-on-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/type :story
                                     :task/meta {:refined "true"}}))
          (fn [_dir _id] [(make-child)])
          (let [result (graph/query [`(resolvers/work-on!
                                       {:task/project-dir "/my/project"
                                        :task/id 555})])
                work-on-result (get result `resolvers/work-on!)
                session-id (:work-on/session-id work-on-result)
                data (sc/get-data session-id)]
            (is (= "/my/project" (:project-dir data)))
            (is (= 555 (:task-id data)))
            (is (= :story (:task-type data)))
            (is (= session-id (:session-id data)))))))))

;;; Invoke Skill Tests

(deftest invoke-skill-test
  ;; Verify invoke-skill! starts claude-cli and returns immediately.
  ;; Uses virtual thread to wait for completion.
  (testing "invoke-skill!"
    (testing "invokes claude-cli with skill prompt"
      (with-work-on-state
        (let [invoked-opts (atom nil)
              mock-promise (promise)]
          (with-mock-fetchers
            (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
            (fn [_dir _id] [])
            (with-redefs [claude-cli/invoke (fn [opts]
                                              (reset! invoked-opts opts)
                                              {:result-promise mock-promise})]
              ;; Start a work-on session to get valid session-id with data
              (let [work-result (graph/query [`(resolvers/work-on!
                                                {:task/project-dir "/test/dir"
                                                 :task/id 99})])
                    session-id (:work-on/session-id (get work-result `resolvers/work-on!))
                    ;; Now invoke skill
                    result (graph/query [`(resolvers/invoke-skill!
                                           {:skill "mcp-tasks:refine-task"
                                            :engine/session-id ~session-id})])
                    invoke-result (get result `resolvers/invoke-skill!)]
                (is (= :started (:invoke-skill/status invoke-result)))
                (is (= "/mcp-tasks:refine-task" (:prompt @invoked-opts)))
                (is (= "/test/dir" (:dir @invoked-opts)))
                ;; Deliver the promise to clean up virtual thread
                (deliver mock-promise {:exit-code 0 :events []})))))))

    (testing "sends error event on skill failure"
      (with-work-on-state
        (let [mock-promise (promise)
              events-sent (atom [])]
          (with-mock-fetchers
            (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
            (fn [_dir _id] [])
            (with-redefs [claude-cli/invoke (fn [_opts]
                                              {:result-promise mock-promise})
                          sc/send! (fn [sid event]
                                     (swap! events-sent conj {:session-id sid :event event})
                                     #{:escalated})]
              (let [work-result (graph/query [`(resolvers/work-on!
                                                {:task/project-dir "/test"
                                                 :task/id 100})])
                    session-id (:work-on/session-id (get work-result `resolvers/work-on!))
                    _ (graph/query [`(resolvers/invoke-skill!
                                      {:skill "mcp-tasks:refine-task"
                                       :engine/session-id ~session-id})])]
                ;; Deliver error result
                (deliver mock-promise {:error :timeout})
                ;; Wait for virtual thread to process
                (Thread/sleep 100)
                (is (= 1 (count @events-sent)))
                (is (= :error (:event (first @events-sent))))))))))))

;;; Concurrent Skill Invocation Tests

(deftest concurrent-skill-invocation-test
  ;; Verify multiple concurrent skill invocations are handled correctly.
  ;; Tests thread safety and proper state management.
  (testing "concurrent skill invocations"
    (testing "handles multiple skills invoked in parallel"
      (with-work-on-state
        (let [invoke-count (atom 0)
              promises (atom [])
              invoked-skills (atom [])]
          (with-mock-fetchers
            (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
            (fn [_dir _id] [])
            (with-redefs [claude-cli/invoke (fn [opts]
                                              (swap! invoke-count inc)
                                              (swap! invoked-skills conj (:prompt opts))
                                              (let [p (promise)]
                                                (swap! promises conj p)
                                                {:result-promise p}))]
              ;; Start a work-on session
              (let [work-result (graph/query [`(resolvers/work-on!
                                                {:task/project-dir "/test"
                                                 :task/id 200})])
                    session-id (:work-on/session-id (get work-result `resolvers/work-on!))]
                ;; Invoke multiple skills concurrently
                (dotimes [i 3]
                  (graph/query [`(resolvers/invoke-skill!
                                  {:skill ~(str "skill-" i)
                                   :engine/session-id ~session-id})]))
                ;; All three should have been invoked
                (is (= 3 @invoke-count))
                (is (= #{"/skill-0" "/skill-1" "/skill-2"} (set @invoked-skills)))
                ;; Clean up virtual threads
                (doseq [p @promises]
                  (deliver p {:exit-code 0 :events []}))))))))

    (testing "each concurrent invocation gets correct session data"
      (with-work-on-state
        (let [captured-dirs (atom [])]
          (with-mock-fetchers
            (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
            (fn [_dir _id] [])
            (with-redefs [claude-cli/invoke (fn [opts]
                                              (swap! captured-dirs conj (:dir opts))
                                              {:result-promise (doto (promise)
                                                                 (deliver {:exit-code 0}))})]
              ;; Start two sessions with different project dirs
              (let [result1 (graph/query [`(resolvers/work-on!
                                            {:task/project-dir "/project-a"
                                             :task/id 301})])
                    session1 (:work-on/session-id (get result1 `resolvers/work-on!))
                    result2 (graph/query [`(resolvers/work-on!
                                            {:task/project-dir "/project-b"
                                             :task/id 302})])
                    session2 (:work-on/session-id (get result2 `resolvers/work-on!))]
                ;; Invoke skills on both sessions
                (graph/query [`(resolvers/invoke-skill!
                                {:skill "test"
                                 :engine/session-id ~session1})])
                (graph/query [`(resolvers/invoke-skill!
                                {:skill "test"
                                 :engine/session-id ~session2})])
                ;; Verify each got the right project dir
                (is (= #{"/project-a" "/project-b"} (set @captured-dirs)))))))))))

;;; Escalate to Dev-env Tests

(deftest escalate-to-dev-env-test
  ;; Verify escalate-to-dev-env! starts a dev-env session for human intervention.
  (testing "escalate-to-dev-env!"
    (testing "starts dev-env session when available"
      (with-work-on-state
        (let [dev-env (dev-env-protocol/make-noop-dev-env)
              dev-env-id (dev-env-registry/register! dev-env :test)]
          (with-mock-fetchers
            (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
            (fn [_dir _id] [])
            (let [work-result (graph/query [`(resolvers/work-on!
                                              {:task/project-dir "/test"
                                               :task/id 200})])
                  session-id (:work-on/session-id (get work-result `resolvers/work-on!))
                  result (graph/query [`(resolvers/escalate-to-dev-env!
                                         {:engine/session-id ~session-id})])
                  escalate-result (get result `resolvers/escalate-to-dev-env!)
                  ;; Check that start-session was called via the :calls atom
                  calls @(:calls dev-env)
                  start-call (first (filter #(= :start-session (:op %)) calls))]
              (is (= :escalated (:escalate/status escalate-result)))
              (is (= dev-env-id (:escalate/dev-env-id escalate-result)))
              (is (some? start-call) "start-session should have been called")
              (is (= session-id (:session-id start-call))))))))

    (testing "returns error when no dev-env available"
      (with-work-on-state
        (with-mock-fetchers
          (fn [_dir _id] (make-task {:task/meta {:refined "true"}}))
          (fn [_dir _id] [])
          (let [work-result (graph/query [`(resolvers/work-on!
                                            {:task/project-dir "/test"
                                             :task/id 201})])
                session-id (:work-on/session-id (get work-result `resolvers/work-on!))
                result (graph/query [`(resolvers/escalate-to-dev-env!
                                       {:engine/session-id ~session-id})])
                escalate-result (get result `resolvers/escalate-to-dev-env!)]
            (is (= :no-dev-env (:escalate/status escalate-result)))
            (is (= :no-dev-env (:error (:escalate/error escalate-result))))))))))
