(ns task-conductor.statechart-engine.interface-test
  ;; Verify the public interface provides complete statechart functionality
  ;; without requiring direct imports from fulcrologic/statecharts.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.test-helpers :refer [with-clean-engine]]))

(def traffic-light
  "Three-state traffic light: :red -> :green -> :yellow -> :red"
  (sc/statechart {}
                 (sc/state {:id :red}
                           (sc/transition {:event :next :target :green}))
                 (sc/state {:id :green}
                           (sc/transition {:event :next :target :yellow}))
                 (sc/state {:id :yellow}
                           (sc/transition {:event :next :target :red}))))

(deftest interface-provides-complete-dsl
  ;; Uses only interface imports to build and run a statechart,
  ;; verifying consumers don't need fulcrologic namespace imports.
  (testing "interface namespace"
    (testing "allows building a statechart with state and transition"
      (with-clean-engine
        (let [result (sc/register! ::traffic traffic-light)]
          (is (= {:ok ::traffic} result)))))

    (testing "supports full session lifecycle via interface"
      (with-clean-engine
        (sc/register! ::traffic traffic-light)
        (let [{:keys [ok]} (sc/start! ::traffic)
              session-id   ok]
          (is (string? session-id))
          (is (= {:ok #{:red}} (sc/current-state session-id)))
          (is (= #{:next} (:ok (sc/available-events session-id))))
          (sc/send! session-id :next)
          (is (= {:ok #{:green}} (sc/current-state session-id)))
          (sc/send! session-id :next)
          (is (= {:ok #{:yellow}} (sc/current-state session-id)))
          (sc/send! session-id :next)
          (is (= {:ok #{:red}} (sc/current-state session-id)))
          (is (= {:ok session-id} (sc/stop! session-id))))))

    (testing "provides introspection through interface"
      (with-clean-engine
        (sc/register! ::chart-a traffic-light)
        (sc/register! ::chart-b traffic-light)
        (is (= 2 (count (:ok (sc/list-charts)))))
        (let [{s1 :ok} (sc/start! ::chart-a)
              _        (sc/start! ::chart-b)]
          (is (= 2 (count (:ok (sc/list-sessions)))))
          (sc/send! s1 :next)
          (let [{:keys [ok]} (sc/history s1)]
            (is (= 2 (count ok)))
            (is (= #{:red} (:state (first ok))))
            (is (= #{:green} (:state (second ok))))))))))

(deftest interface-re-exports-dsl-elements
  ;; Verifies all expected DSL elements are available.
  (testing "interface re-exports"
    (testing "chart construction elements are defined"
      (is (fn? sc/statechart))
      (is (fn? sc/state))
      (is (fn? sc/transition))
      (is (fn? sc/initial))
      (is (fn? sc/final))
      (is (fn? sc/parallel))
      (is (fn? sc/history-node))
      (is (fn? sc/on-entry))
      (is (fn? sc/on-exit))
      (is (fn? sc/assign))
      (is (fn? sc/Send)))

    (testing "engine functions are defined"
      (is (fn? sc/register!))
      (is (fn? sc/unregister!))
      (is (fn? sc/start!))
      (is (fn? sc/send!))
      (is (fn? sc/stop!))
      (is (fn? sc/reset-engine!)))

    (testing "introspection functions are defined"
      (is (fn? sc/current-state))
      (is (fn? sc/list-sessions))
      (is (fn? sc/list-charts))
      (is (fn? sc/available-events))
      (is (fn? sc/history)))))
