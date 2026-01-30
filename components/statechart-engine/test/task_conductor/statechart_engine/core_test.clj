(ns task-conductor.statechart-engine.core-test
  ;; Verify component scaffold and dependencies are correctly configured.
  ;; Loading this namespace exercises the deps.edn configuration.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.statechart-engine.core]))

(deftest core-namespace-loads
  (testing "task-conductor.statechart-engine.core"
    (testing "loads successfully"
      (is (find-ns 'task-conductor.statechart-engine.core)))))
