(ns task-conductor.claude-cli-test.registry-test
  "Tests for the invocation registry."
  (:require [clojure.test :refer [deftest is testing]]
            [task-conductor.claude-cli.registry :as registry]))

;; Tests for create-invocation!, get-invocation, and update-invocation!.
;; Contracts: create returns UUID, get retrieves entry, update modifies fields,
;; missing IDs return nil.

(defmacro with-clean-registry
  "Execute body with an empty registry, cleaning up afterward."
  [& body]
  `(do
     (registry/clear-registry!)
     (try
       ~@body
       (finally
         (registry/clear-registry!)))))

(deftest create-invocation!-test
  (testing "create-invocation!"
    (testing "returns a UUID"
      (with-clean-registry
        (let [id (registry/create-invocation! {:mock "handle"})]
          (is (uuid? id)))))

    (testing "returns different UUIDs for each call"
      (with-clean-registry
        (let [id1 (registry/create-invocation! {:handle 1})
              id2 (registry/create-invocation! {:handle 2})]
          (is (not= id1 id2)))))

    (testing "stores the handle in the entry"
      (with-clean-registry
        (let [handle {:process :mock :result-promise :mock}
              id (registry/create-invocation! handle)
              entry (registry/get-invocation id)]
          (is (= handle (:handle entry))))))

    (testing "initializes entry with :status :pending"
      (with-clean-registry
        (let [id (registry/create-invocation! {:mock "handle"})
              entry (registry/get-invocation id)]
          (is (= :pending (:status entry))))))

    (testing "initializes entry with :result nil"
      (with-clean-registry
        (let [id (registry/create-invocation! {:mock "handle"})
              entry (registry/get-invocation id)]
          (is (nil? (:result entry))))))))

(deftest get-invocation-test
  (testing "get-invocation"
    (testing "retrieves an existing entry"
      (with-clean-registry
        (let [handle {:test "data"}
              id (registry/create-invocation! handle)
              entry (registry/get-invocation id)]
          (is (some? entry))
          (is (= handle (:handle entry))))))

    (testing "returns nil for non-existent ID"
      (with-clean-registry
        (let [fake-id (random-uuid)]
          (is (nil? (registry/get-invocation fake-id))))))))

(deftest update-invocation!-test
  (testing "update-invocation!"
    (testing "updates :status field"
      (with-clean-registry
        (let [id (registry/create-invocation! {:mock "handle"})
              updated (registry/update-invocation! id {:status :complete})]
          (is (= :complete (:status updated)))
          (is (= :complete (:status (registry/get-invocation id)))))))

    (testing "updates :result field"
      (with-clean-registry
        (let [id (registry/create-invocation! {:mock "handle"})
              result {:exit-code 0 :events [{:type "end"}]}
              updated (registry/update-invocation! id {:result result})]
          (is (= result (:result updated)))
          (is (= result (:result (registry/get-invocation id)))))))

    (testing "updates multiple fields at once"
      (with-clean-registry
        (let [id (registry/create-invocation! {:mock "handle"})
              result {:exit-code 1}
              updated (registry/update-invocation! id {:status :error
                                                       :result result})]
          (is (= :error (:status updated)))
          (is (= result (:result updated))))))

    (testing "preserves existing fields not being updated"
      (with-clean-registry
        (let [handle {:original "handle"}
              id (registry/create-invocation! handle)
              _ (registry/update-invocation! id {:status :complete})
              entry (registry/get-invocation id)]
          (is (= handle (:handle entry))))))

    (testing "returns nil for non-existent ID"
      (with-clean-registry
        (let [fake-id (random-uuid)]
          (is
           (nil?
            (registry/update-invocation! fake-id {:status :complete}))))))))

(deftest remove-invocation!-test
  (testing "remove-invocation!"
    (testing "removes an existing entry"
      (with-clean-registry
        (let [id (registry/create-invocation! {:mock "handle"})]
          (registry/remove-invocation! id)
          (is (nil? (registry/get-invocation id))))))

    (testing "returns the removed entry"
      (with-clean-registry
        (let [handle {:test "data"}
              id (registry/create-invocation! handle)
              removed (registry/remove-invocation! id)]
          (is (some? removed))
          (is (= handle (:handle removed))))))

    (testing "returns nil for non-existent ID"
      (with-clean-registry
        (let [fake-id (random-uuid)]
          (is (nil? (registry/remove-invocation! fake-id))))))))
