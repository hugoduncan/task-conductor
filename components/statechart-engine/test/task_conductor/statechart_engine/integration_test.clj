(ns task-conductor.statechart-engine.integration-test
  ;; Integration tests for the statechart engine
  ;; using a realistic traffic light scenario.
  ;; Tests multiple states, history pseudo-state,
  ;; entry/exit actions, introspection,
  ;; and concurrent sessions.
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.test-helpers :refer [with-clean-engine]]))

;;; Traffic Light Statechart Definition
;; States: :red, :green, :yellow (normal operation) and :maintenance
;; Transitions: :next cycles through lights, :maintain enters maintenance,
;; :resume returns via shallow history to previous state.

(defn make-traffic-light
  "Creates a traffic light chart with action tracking atom."
  [action-log]
  (sc/statechart {}
                 (sc/state {:id :operating}
                           (sc/initial {}
                                       (sc/transition {:target :red}))
                           (sc/state {:id :red}
                                     (sc/on-entry {}
                                                  (sc/assign
                                                   {:location
                                                    [:enter-count :red]
                                                    :expr
                                                    (fn [_env data]
                                                      (swap!
                                                       action-log
                                                       conj
                                                       [:enter
                                                        :red])
                                                      (inc
                                                       (get-in
                                                        data
                                                        [:enter-count
                                                         :red]
                                                        0)))}))
                                     (sc/on-exit {}
                                                 (sc/assign
                                                  {:location [:exit-count :red]
                                                   :expr     (fn [_env data]
                                                               (swap!
                                                                action-log
                                                                conj
                                                                [:exit
                                                                 :red])
                                                               (inc
                                                                (get-in
                                                                 data
                                                                 [:exit-count
                                                                  :red]
                                                                 0)))}))
                                     (sc/transition
                                      {:event :next :target :green}))
                           (sc/state {:id :green}
                                     (sc/on-entry {}
                                                  (sc/assign
                                                   {:location
                                                    [:enter-count :green]
                                                    :expr
                                                    (fn [_env data]
                                                      (swap!
                                                       action-log
                                                       conj
                                                       [:enter
                                                        :green])
                                                      (inc
                                                       (get-in
                                                        data
                                                        [:enter-count
                                                         :green]
                                                        0)))}))
                                     (sc/on-exit {}
                                                 (sc/assign
                                                  {:location
                                                   [:exit-count :green]
                                                   :expr
                                                   (fn [_env data]
                                                     (swap!
                                                      action-log
                                                      conj
                                                      [:exit
                                                       :green])
                                                     (inc
                                                      (get-in
                                                       data
                                                       [:exit-count
                                                        :green]
                                                       0)))}))
                                     (sc/transition
                                      {:event :next :target :yellow}))
                           (sc/state {:id :yellow}
                                     (sc/on-entry {}
                                                  (sc/assign
                                                   {:location
                                                    [:enter-count :yellow]
                                                    :expr
                                                    (fn [_env data]
                                                      (swap!
                                                       action-log
                                                       conj
                                                       [:enter
                                                        :yellow])
                                                      (inc
                                                       (get-in
                                                        data
                                                        [:enter-count
                                                         :yellow]
                                                        0)))}))
                                     (sc/on-exit {}
                                                 (sc/assign
                                                  {:location
                                                   [:exit-count :yellow]
                                                   :expr
                                                   (fn [_env data]
                                                     (swap!
                                                      action-log
                                                      conj
                                                      [:exit
                                                       :yellow])
                                                     (inc
                                                      (get-in
                                                       data
                                                       [:exit-count
                                                        :yellow]
                                                       0)))}))
                                     (sc/transition
                                      {:event :next :target :red}))
                           (sc/history-node
                            {:id :operating-history :type :shallow}
                            (sc/transition {:target :red}))
                           (sc/transition
                            {:event :maintain :target :maintenance}))
                 (sc/state {:id :maintenance}
                           (sc/on-entry {}
                                        (sc/assign
                                         {:location [:enter-count :maintenance]
                                          :expr     (fn [_env data]
                                                      (swap!
                                                       action-log
                                                       conj
                                                       [:enter
                                                        :maintenance])
                                                      (inc
                                                       (get-in
                                                        data
                                                        [:enter-count
                                                         :maintenance]
                                                        0)))}))
                           (sc/on-exit {}
                                       (sc/assign
                                        {:location [:exit-count :maintenance]
                                         :expr     (fn [_env data]
                                                     (swap!
                                                      action-log
                                                      conj
                                                      [:exit
                                                       :maintenance])
                                                     (inc
                                                      (get-in
                                                       data
                                                       [:exit-count
                                                        :maintenance]
                                                       0)))}))
                           (sc/transition
                            {:event :resume :target :operating-history}))))

(defn simple-traffic-light
  "Simple traffic light without action tracking for basic tests."
  []
  (sc/statechart {}
                 (sc/state {:id :operating}
                           (sc/initial {}
                                       (sc/transition {:target :red}))
                           (sc/state {:id :red}
                                     (sc/transition
                                      {:event :next :target :green}))
                           (sc/state {:id :green}
                                     (sc/transition
                                      {:event :next :target :yellow}))
                           (sc/state {:id :yellow}
                                     (sc/transition
                                      {:event :next :target :red}))
                           (sc/history-node
                            {:id :operating-history :type :shallow}
                            (sc/transition {:target :red}))
                           (sc/transition
                            {:event :maintain :target :maintenance}))
                 (sc/state {:id :maintenance}
                           (sc/transition
                            {:event :resume :target :operating-history}))))

;;; Tests

(deftest full-cycle-test
  ;; Tests complete traffic light cycle: red → green → yellow → red.
  (testing "full cycle"
    (testing "cycles through red → green → yellow → red"
      (with-clean-engine
        (sc/register! ::traffic (simple-traffic-light))
        (let [sid (sc/start! ::traffic)]
          (is (contains? (sc/current-state sid) :red))
          (sc/send! sid :next)
          (is (contains? (sc/current-state sid) :green))
          (sc/send! sid :next)
          (is (contains? (sc/current-state sid) :yellow))
          (sc/send! sid :next)
          (is (contains? (sc/current-state sid) :red)))))))

(deftest maintenance-interrupt-test
  ;; Tests maintenance mode: interrupt from any state, return via history.
  (testing "maintenance interrupt"
    (testing "can enter maintenance from green and return via history"
      (with-clean-engine
        (sc/register! ::maint (simple-traffic-light))
        (let [sid (sc/start! ::maint)]
          ;; Move to green
          (sc/send! sid :next)
          (is (contains? (sc/current-state sid) :green))
          ;; Enter maintenance
          (sc/send! sid :maintain)
          (is (contains? (sc/current-state sid) :maintenance))
          ;; Resume should return to green via history
          (sc/send! sid :resume)
          (is (contains? (sc/current-state sid) :green)))))

    (testing "can enter maintenance from yellow and return via history"
      (with-clean-engine
        (sc/register! ::maint2 (simple-traffic-light))
        (let [sid (sc/start! ::maint2)]
          ;; Move to yellow
          (sc/send! sid :next)
          (sc/send! sid :next)
          (is (contains? (sc/current-state sid) :yellow))
          ;; Enter maintenance
          (sc/send! sid :maintain)
          (is (contains? (sc/current-state sid) :maintenance))
          ;; Resume should return to yellow
          (sc/send! sid :resume)
          (is (contains? (sc/current-state sid) :yellow)))))))

(deftest introspection-during-execution-test
  ;; Tests introspection functions at each step of execution.
  (testing "introspection during execution"
    (testing
     "current-state, available-events, and history are accurate at each step"
      (with-clean-engine
        (sc/register! ::intro (simple-traffic-light))
        (let [sid (sc/start! ::intro)]
          ;; Initial state
          (is (= #{:operating :red} (sc/current-state sid)))
          (is (contains? (sc/available-events sid) :next))
          (is (contains? (sc/available-events sid) :maintain))
          (is (= 1 (count (sc/history sid))))
          (is (nil? (:event (first (sc/history sid)))))

          ;; After first transition
          (sc/send! sid :next)
          (is (= #{:operating :green} (sc/current-state sid)))
          (is (= 2 (count (sc/history sid))))
          (is (= :next (:event (second (sc/history sid)))))

          ;; After maintenance
          (sc/send! sid :maintain)
          (is (= #{:maintenance} (sc/current-state sid)))
          (is (= #{:resume} (sc/available-events sid)))
          (is (= 3 (count (sc/history sid))))

          ;; After resume
          (sc/send! sid :resume)
          (is (= #{:operating :green} (sc/current-state sid)))
          (is (= 4 (count (sc/history sid)))))))))

(deftest concurrent-sessions-test
  ;; Tests multiple sessions of the same chart running independently.
  (testing "concurrent sessions"
    (testing "multiple sessions run independently"
      (with-clean-engine
        (sc/register! ::concurrent (simple-traffic-light))
        (let [s1 (sc/start! ::concurrent)
              s2 (sc/start! ::concurrent)
              s3 (sc/start! ::concurrent)]
          ;; All start at red
          (is (contains? (sc/current-state s1) :red))
          (is (contains? (sc/current-state s2) :red))
          (is (contains? (sc/current-state s3) :red))

          ;; Move s1 to green, s2 to yellow, leave s3 at red
          (sc/send! s1 :next)
          (sc/send! s2 :next)
          (sc/send! s2 :next)

          ;; Verify independent states
          (is (contains? (sc/current-state s1) :green))
          (is (contains? (sc/current-state s2) :yellow))
          (is (contains? (sc/current-state s3) :red))

          ;; Verify independent histories
          (is (= 2 (count (sc/history s1))))
          (is (= 3 (count (sc/history s2))))
          (is (= 1 (count (sc/history s3))))

          ;; Verify list-sessions shows all three
          (let [sessions (sc/list-sessions)]
            (is (= 3 (count sessions)))
            (is (some #{s1} sessions))
            (is (some #{s2} sessions))
            (is (some #{s3} sessions))))))))

(deftest entry-exit-actions-test
  ;; Tests that entry/exit actions execute and track properly.
  (testing "entry/exit actions"
    (testing "execute on state transitions"
      (with-clean-engine
        (let [action-log (atom [])
              chart      (make-traffic-light action-log)]
          (sc/register! ::actions chart)
          (let [sid (sc/start! ::actions)]
            ;; Initial entry to red
            (is (= [[:enter :red]] @action-log))

            ;; Transition red -> green
            (sc/send! sid :next)
            (is (= [[:enter :red] [:exit :red] [:enter :green]] @action-log))

            ;; Transition green -> yellow
            (sc/send! sid :next)
            (is (= [[:enter :red] [:exit :red] [:enter :green]
                    [:exit :green] [:enter :yellow]] @action-log))

            ;; Transition yellow -> red (completing cycle)
            (sc/send! sid :next)
            (is (= [[:enter :red] [:exit :red] [:enter :green]
                    [:exit :green] [:enter :yellow]
                    [:exit :yellow] [:enter :red]] @action-log))))))

    (testing "execute on maintenance interrupt and resume"
      (with-clean-engine
        (let [action-log (atom [])
              chart      (make-traffic-light action-log)]
          (sc/register! ::maint-actions chart)
          (let [sid (sc/start! ::maint-actions)]
            ;; Move to green
            (sc/send! sid :next)
            (reset! action-log [])

            ;; Enter maintenance
            (sc/send! sid :maintain)
            (is (= [[:exit :green] [:enter :maintenance]] @action-log))

            ;; Resume
            (sc/send! sid :resume)
            (is (= [[:exit :green] [:enter :maintenance]
                    [:exit :maintenance] [:enter :green]] @action-log))))))))
