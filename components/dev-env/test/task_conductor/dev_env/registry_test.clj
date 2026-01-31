(ns task-conductor.dev-env.registry-test
  ;; Verify registry functions correctly manage dev-env instances,
  ;; including registration lifecycle, lookup, listing, and selection.
  (:require
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.dev-env.protocol :as protocol]
   [task-conductor.dev-env.registry :as registry]))

(defmacro with-clear-registry
  "Execute body with a clean registry, ensuring cleanup afterwards."
  [& body]
  `(do
     (registry/clear!)
     (try
       ~@body
       (finally
         (registry/clear!)))))

(deftest register-test
  (with-clear-registry
    (testing "register!"
      (testing "returns a string ID"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :noop)]
          (is (string? id))
          (is (str/starts-with? id "dev-env-"))))

      (testing "generates unique IDs for each registration"
        (let [dev-env1 (protocol/make-noop-dev-env)
              dev-env2 (protocol/make-noop-dev-env)
              id1 (registry/register! dev-env1 :noop)
              id2 (registry/register! dev-env2 :noop)]
          (is (not= id1 id2))))

      (testing "accepts optional metadata"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :emacs {:host "localhost"})]
          (is (string? id))
          (is (= {:host "localhost"} (:meta (registry/get-dev-env-entry id)))))))))

(deftest unregister-test
  (with-clear-registry
    (testing "unregister!"
      (testing "removes existing dev-env and returns true"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :noop)]
          (is (some? (registry/get-dev-env id)))
          (is (true? (registry/unregister! id)))
          (is (nil? (registry/get-dev-env id)))))

      (testing "returns false for non-existent ID"
        (is (false? (registry/unregister! "dev-env-nonexistent")))))))

(deftest get-dev-env-test
  (with-clear-registry
    (testing "get-dev-env"
      (testing "returns the DevEnv instance for existing ID"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :noop)]
          (is (= dev-env (registry/get-dev-env id)))))

      (testing "returns nil for non-existent ID"
        (is (nil? (registry/get-dev-env "dev-env-missing")))))))

(deftest get-dev-env-entry-test
  (with-clear-registry
    (testing "get-dev-env-entry"
      (testing "returns full entry map for existing ID"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :terminal {:port 8080})]
          (is (= {:dev-env dev-env
                  :type :terminal
                  :meta {:port 8080}}
                 (registry/get-dev-env-entry id)))))

      (testing "returns nil for non-existent ID"
        (is (nil? (registry/get-dev-env-entry "dev-env-missing")))))))

(deftest list-dev-envs-test
  (with-clear-registry
    (testing "list-dev-envs"
      (testing "returns empty vector when registry is empty"
        (is (= [] (registry/list-dev-envs))))

      (testing "returns all registered dev-envs with id, type, and meta"
        (let [dev-env1 (protocol/make-noop-dev-env)
              dev-env2 (protocol/make-noop-dev-env)
              id1 (registry/register! dev-env1 :emacs {:name "emacs1"})
              id2 (registry/register! dev-env2 :terminal {})
              listed (registry/list-dev-envs)
              by-id (into {} (map (juxt :id identity)) listed)]
          (is (= 2 (count listed)))
          (is (= {:id id1 :type :emacs :meta {:name "emacs1"}}
                 (get by-id id1)))
          (is (= {:id id2 :type :terminal :meta {}}
                 (get by-id id2)))))

      (testing "does not include dev-env instances"
        (let [dev-env (protocol/make-noop-dev-env)
              _ (registry/register! dev-env :noop)
              listed (first (registry/list-dev-envs))]
          (is (not (contains? listed :dev-env))))))))

(deftest select-dev-env-test
  (with-clear-registry
    (testing "select-dev-env"
      (testing "returns nil when registry is empty"
        (is (nil? (registry/select-dev-env))))

      (testing "returns entry with dev-env instance when populated"
        (let [dev-env (protocol/make-noop-dev-env)
              id (registry/register! dev-env :emacs {:host "localhost"})
              selected (registry/select-dev-env)]
          (is (= id (:id selected)))
          (is (= dev-env (:dev-env selected)))
          (is (= :emacs (:type selected)))
          (is (= {:host "localhost"} (:meta selected)))))

      (testing "returns a dev-env when multiple are registered"
        (let [dev-env1 (protocol/make-noop-dev-env)
              dev-env2 (protocol/make-noop-dev-env)
              _ (registry/register! dev-env1 :emacs)
              _ (registry/register! dev-env2 :terminal)
              selected (registry/select-dev-env)]
          (is (some? (:id selected)))
          (is (some? (:dev-env selected))))))))

(deftest clear-test
  (with-clear-registry
    (testing "clear!"
      (testing "removes all entries from registry"
        (let [dev-env1 (protocol/make-noop-dev-env)
              dev-env2 (protocol/make-noop-dev-env)
              _ (registry/register! dev-env1 :emacs)
              _ (registry/register! dev-env2 :terminal)]
          (is (= 2 (count (registry/list-dev-envs))))
          (registry/clear!)
          (is (= [] (registry/list-dev-envs))))))))
