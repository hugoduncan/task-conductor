(ns task-conductor.pathom-graph.core-test
  ;; Tests for the pathom-graph core functionality.
  ;; Verifies: registration, env caching, cache invalidation, query execution.
  (:require
   [clojure.test :refer [deftest is testing use-fixtures]]
   [task-conductor.pathom-graph.core :as core]
   [com.wsscode.pathom3.connect.operation :as pco]))

(defn reset-fixture [f]
  (core/reset-graph!)
  (f)
  (core/reset-graph!))

(use-fixtures :each reset-fixture)

(pco/defresolver greeting []
  {::pco/output [:greeting]}
  {:greeting "hello"})

(pco/defresolver user-name [{:keys [user/id]}]
  {::pco/input [:user/id]
   ::pco/output [:user/name]}
  {:user/name (str "user-" id)})

(pco/defmutation set-value! [{:keys [value]}]
  {::pco/output [:result]}
  {:result value})

(deftest register!-test
  (testing "register!"
    (testing "registers a single resolver"
      (is (= :registered (core/register! greeting))))

    (testing "registers multiple resolvers"
      (is (= :registered (core/register! [greeting user-name]))))

    (testing "registers mutations"
      (is (= :registered (core/register! set-value!))))))

(deftest env-test
  (testing "env"
    (testing "returns a Pathom environment"
      (core/register! greeting)
      (let [e (core/env)]
        (is (map? e))
        (is (contains? e :com.wsscode.pathom3.connect.indexes/index-oir))))

    (testing "caches the environment"
      (core/register! greeting)
      (let [e1 (core/env)
            e2 (core/env)]
        (is (identical? e1 e2))))

    (testing "invalidates cache on new registration"
      (core/register! greeting)
      (let [e1 (core/env)]
        (core/register! user-name)
        (let [e2 (core/env)]
          (is (not (identical? e1 e2))))))))

(deftest query-test
  (testing "query"
    (testing "executes simple resolver"
      (core/register! greeting)
      (is (= {:greeting "hello"} (core/query [:greeting]))))

    (testing "executes resolver with entity input"
      (core/register! user-name)
      (is (= {:user/name "user-42"}
             (core/query {:user/id 42} [:user/name]))))

    (testing "executes mutations"
      (core/register! set-value!)
      ;; Mutations use fully-qualified symbol as key (not the invocation form)
      (let [result (core/query ['(task-conductor.pathom-graph.core-test/set-value!
                                  {:value "test"})])]
        (is (= {:result "test"}
               (get result 'task-conductor.pathom-graph.core-test/set-value!)))))))

(deftest reset-graph!-test
  (testing "reset-graph!"
    (testing "clears all registrations"
      (core/register! greeting)
      (is (= {:greeting "hello"} (core/query [:greeting])))
      (core/reset-graph!)
      ;; After reset, env has no resolvers so querying throws
      (is (thrown-with-msg? clojure.lang.ExceptionInfo
                            #"Error while processing request"
                            (core/query [:greeting]))))))
