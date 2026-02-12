(ns task-conductor.dev-env.resolvers-test
  ;; Verify resolvers and mutations correctly delegate to registry and protocol.
  ;; Each test confirms the resolver/mutation calls the expected functions
  ;; and returns properly formatted results.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.protocol :as protocol]
   [task-conductor.dev-env.registry :as registry]
   [task-conductor.dev-env.resolvers :as resolvers]
   [task-conductor.dev-env.test-helpers :refer [with-clean-dev-env-state]]
   [task-conductor.pathom-graph.interface :as graph]))

;;; Resolver Tests

(deftest dev-env-list-test
  (with-clean-dev-env-state
    (testing "dev-env-list"
      (testing "returns empty list when no dev-envs registered"
        (let [result (graph/query [:dev-env/available])]
          (is (= [] (:dev-env/available result)))))

      (testing "returns all registered dev-envs"
        (let [dev-env1 (protocol/make-noop-dev-env)
              dev-env2 (protocol/make-noop-dev-env)
              id1 (registry/register! dev-env1 :emacs {:name "e1"})
              id2 (registry/register! dev-env2 :terminal)
              result (graph/query [:dev-env/available])
              available (:dev-env/available result)
              by-id (into {} (map (juxt :dev-env/id identity)) available)]
          (is (= 2 (count available)))
          (is (= {:dev-env/id id1 :type :emacs :meta {:name "e1"}}
                 (get by-id id1)))
          (is (= {:dev-env/id id2 :type :terminal :meta {}}
                 (get by-id id2))))))))

(deftest dev-env-selected-test
  (with-clean-dev-env-state
    (testing "dev-env-selected"
      (testing "returns nil when no dev-envs registered"
        (let [result (graph/query [:dev-env/selected])]
          (is (nil? (:dev-env/selected result)))))

      (testing "returns selected dev-env when available"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :emacs {:host "local"})
              result (graph/query [:dev-env/selected])
              selected (:dev-env/selected result)]
          (is (= id (:dev-env/id selected)))
          (is (= :emacs (:type selected)))
          (is (= {:host "local"} (:meta selected)))
          (is (some? (:dev-env selected))))))))

(deftest dev-env-by-id-test
  (with-clean-dev-env-state
    (testing "dev-env-by-id"
      (testing "returns dev-env details for existing ID"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :emacs)
              result (graph/query {:dev-env/id id}
                                  [:dev-env/instance
                                   :dev-env/type
                                   :dev-env/connected?])]
          (is (= dev-env (:dev-env/instance result)))
          (is (= :emacs (:dev-env/type result)))
          (is (true? (:dev-env/connected? result)))))

      (testing "returns nil/false for non-existent ID"
        (let [result (graph/query {:dev-env/id "missing"}
                                  [:dev-env/instance
                                   :dev-env/type
                                   :dev-env/connected?])]
          (is (nil? (:dev-env/instance result)))
          (is (nil? (:dev-env/type result)))
          (is (false? (:dev-env/connected? result))))))))

;;; Mutation Tests

(deftest dev-env-start-session-test
  (with-clean-dev-env-state
    (testing "dev-env-start-session!"
      (testing "calls start-session on the dev-env"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :emacs)
              result (graph/query [`(resolvers/dev-env-start-session!
                                     {:dev-env/id ~id
                                      :dev-env/session-id "sess-123"
                                      :dev-env/opts {:dir "/tmp"}})])
              session-result (get-in
                              result
                              [`resolvers/dev-env-start-session!
                               :dev-env/session-result])
              calls @(:calls dev-env)]
          (is (= "sess-123" (:session-id session-result)))
          (is (= :noop (:handle session-result)))
          (is (= 1 (count (filter #(= :start-session (:op %)) calls))))
          (is
           (=
            {:dir "/tmp"}
            (:opts (first (filter #(= :start-session (:op %)) calls)))))))

      (testing "returns error for non-existent dev-env"
        (let [result (graph/query [`(resolvers/dev-env-start-session!
                                     {:dev-env/id "missing"
                                      :dev-env/session-id "sess-123"})])
              session-result (get-in
                              result
                              [`resolvers/dev-env-start-session!
                               :dev-env/session-result])]
          (is (= :not-found (:error session-result))))))))

(deftest dev-env-close-session-test
  (with-clean-dev-env-state
    (testing "dev-env-close-session!"
      (testing "calls close-session on the dev-env"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :emacs)
              result (graph/query [`(resolvers/dev-env-close-session!
                                     {:dev-env/id ~id
                                      :dev-env/session-id "sess-123"})])
              close-result (get-in
                            result
                            [`resolvers/dev-env-close-session!
                             :dev-env/close-result])
              calls @(:calls dev-env)]
          (is (false? close-result))
          (is (= 1 (count (filter #(= :close-session (:op %)) calls))))))

      (testing "returns error for non-existent dev-env"
        (let [result (graph/query [`(resolvers/dev-env-close-session!
                                     {:dev-env/id "missing"
                                      :dev-env/session-id "sess-123"})])
              close-result (get-in
                            result
                            [`resolvers/dev-env-close-session!
                             :dev-env/close-result])]
          (is (= :not-found (:error close-result))))))))

(deftest dev-env-query-transcript-test
  (with-clean-dev-env-state
    (testing "dev-env-query-transcript!"
      (testing "calls query-transcript on the dev-env"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :emacs)
              result (graph/query [`(resolvers/dev-env-query-transcript!
                                     {:dev-env/id ~id
                                      :dev-env/session-id "sess-123"})])
              transcript (get-in
                          result
                          [`resolvers/dev-env-query-transcript!
                           :dev-env/transcript])
              calls @(:calls dev-env)]
          (is (nil? transcript))
          (is (= 1 (count (filter #(= :query-transcript (:op %)) calls))))))

      (testing "returns error for non-existent dev-env"
        (let [result (graph/query [`(resolvers/dev-env-query-transcript!
                                     {:dev-env/id "missing"
                                      :dev-env/session-id "sess-123"})])
              transcript (get-in
                          result
                          [`resolvers/dev-env-query-transcript!
                           :dev-env/transcript])]
          (is (= :not-found (:error transcript))))))))

(deftest dev-env-query-events-test
  (with-clean-dev-env-state
    (testing "dev-env-query-events!"
      (testing "calls query-events on the dev-env"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :emacs)
              result (graph/query [`(resolvers/dev-env-query-events!
                                     {:dev-env/id ~id
                                      :dev-env/session-id "sess-123"})])
              events (get-in
                      result
                      [`resolvers/dev-env-query-events! :dev-env/events])
              calls @(:calls dev-env)]
          (is (nil? events))
          (is (= 1 (count (filter #(= :query-events (:op %)) calls))))))

      (testing "returns error for non-existent dev-env"
        (let [result (graph/query [`(resolvers/dev-env-query-events!
                                     {:dev-env/id "missing"
                                      :dev-env/session-id "sess-123"})])
              events (get-in
                      result
                      [`resolvers/dev-env-query-events! :dev-env/events])]
          (is (= :not-found (:error events))))))))
