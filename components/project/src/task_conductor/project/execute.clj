(ns task-conductor.project.execute
  "State derivation and statechart definitions for task/story execution.
   Pure functions derive execution state from mcp-tasks data.
   Statecharts orchestrate state-driven automation with skill invocation."
  (:require
   [task-conductor.statechart-engine.interface :as sc]))

;;; Task State Derivation

(defn refined?
  "Check if task/story has been refined.
   Refined status is stored in :meta map with :refined key.
   Handles namespaced keys (e.g. :user/refined from mcp-tasks CLI)."
  [task]
  (let [m (:meta task)]
    (some (fn [[k _]] (= "refined" (name k))) m)))

(defn derive-task-state
  "Derive execution state for a standalone task.

   Returns one of:
   - :unrefined   - needs refinement (:meta :refined is nil)
   - :refined     - ready for execution
   - :done        - executed, awaiting code review
   - :awaiting-pr - reviewed, needs PR creation
   - :wait-pr-merge - PR created, awaiting merge
   - :complete    - task closed

   Task map should contain:
   - :status        - :open, :closed, :in-progress, :blocked, :done
   - :meta          - map with :refined key when refined
   - :code-reviewed - ISO-8601 timestamp when reviewed
   - :pr-num        - GitHub PR number when PR created
   - :pr-merged?    - true when PR has been merged (external check)"
  [task]
  (cond
    (= :closed (:status task))
    :complete

    (and (:pr-num task) (:pr-merged? task))
    :complete

    (:pr-num task)
    :wait-pr-merge

    (and (= :done (:status task)) (:code-reviewed task))
    :awaiting-pr

    (= :done (:status task))
    :done

    (refined? task)
    :refined

    :else
    :unrefined))

;;; Story State Derivation

(defn- active-children
  "Filter out deleted children. Handles both keyword and string status."
  [children]
  (remove #(#{:deleted "deleted"} (:status %)) children))

(defn count-completed-children
  "Count children with closed/done status.
   Monotonically increasing — used to detect progress in :has-tasks state."
  [children]
  (count (filter #(#{:closed "closed" :done "done"} (:status %))
                 (active-children children))))

(defn- children-complete?
  "Check if all active children are complete.
   Handles both keyword and string status equivalents."
  [children]
  (let [active (active-children children)]
    (and (seq active)
         (every? #(#{:closed "closed" :done "done"} (:status %)) active))))

(defn- has-incomplete-children?
  "Check if there are any incomplete active children."
  [children]
  (let [active (active-children children)]
    (and (seq active)
         (some #(not (#{:closed "closed" :done "done"} (:status %))) active))))

(defn derive-story-state
  "Derive execution state for a story.

   Returns one of:
   - :unrefined     - needs refinement (:meta :refined is nil)
   - :refined       - refined but no children created yet
   - :has-tasks     - has incomplete child tasks to execute
   - :done          - all children complete, awaiting code review
   - :awaiting-pr   - reviewed, needs PR creation
   - :wait-pr-merge - PR created, awaiting merge
   - :complete      - story closed

   Story map should contain:
   - :status        - :open, :closed, :in-progress, :blocked
   - :meta          - map with :refined key when refined
   - :code-reviewed - ISO-8601 timestamp when reviewed
   - :pr-num        - GitHub PR number when PR created
   - :pr-merged?    - true when PR has been merged (external check)

   Children is a seq of child task maps. Children are considered complete
   when their status is :done or :closed."
  [story children]
  (cond
    (= :closed (:status story))
    :complete

    (and (:pr-num story) (:pr-merged? story))
    :complete

    (:pr-num story)
    :wait-pr-merge

    (and (children-complete? children) (:code-reviewed story))
    :awaiting-pr

    (children-complete? children)
    :done

    (has-incomplete-children? children)
    :has-tasks

    (refined? story)
    :refined

    :else
    :unrefined))

;;; Statechart Definitions
;; Statecharts orchestrate task/story execution through state-driven automation.
;; States match derive-*-state return values. Transitions are triggered by
;; sending the derived state keyword as an event
;; (e.g., :refined, :has-tasks).
;; Entry actions invoke skills via execute mutations
;; (implemented in later tasks).

;; State sets serve as reference documentation for valid statechart states.
;; Verified by tests to match the actual statechart definitions.

(def task-states
  "Valid states for standalone task execution."
  #{:idle
    :unrefined
    :refined
    :done
    :awaiting-pr
    :wait-pr-merge
    :merging-pr
    :complete
    :terminated
    :escalated
    :session-idle
    :session-running})

(def story-states
  "Valid states for story execution."
  #{:idle :unrefined :refined :has-tasks :done
    :awaiting-pr :wait-pr-merge :merging-pr
    :complete :terminated
    :escalated
    :session-idle :session-running})

;;; Action Expression Defs
;; Extracted to avoid deeply nested long lines in statecharts.
;; Each def is an action map for sc/action.

(def ^:private refine-task-action
  {:expr '(task-conductor.project.resolvers/invoke-skill!
           {:skill "mcp-tasks:refine-task (MCP)"
            :args "{task-id}"})})

(def ^:private execute-task-action
  {:expr '(task-conductor.project.resolvers/invoke-skill!
           {:skill "mcp-tasks:execute-task (MCP)"
            :args "{task-id}"})})

(def ^:private review-task-action
  {:expr
   '(task-conductor.project.resolvers/invoke-skill!
     {:skill
      "mcp-tasks:review-task-implementation (MCP)"
      :args "{task-id}"})})

(def ^:private create-task-pr-action
  {:expr '(task-conductor.project.resolvers/invoke-skill!
           {:skill "mcp-tasks:create-task-pr (MCP)"
            :args "{task-id}"})})

(def ^:private create-story-tasks-action
  {:expr
   '(task-conductor.project.resolvers/invoke-skill!
     {:skill "mcp-tasks:create-story-tasks (MCP)"
      :args "{task-id}"})})

(def ^:private execute-story-child-action
  {:expr
   '(task-conductor.project.resolvers/invoke-skill!
     {:skill
      "mcp-tasks:execute-story-child (MCP)"
      :args "{task-id}"})})

(def ^:private review-story-action
  {:expr
   '(task-conductor.project.resolvers/invoke-skill!
     {:skill
      "mcp-tasks:review-story-implementation (MCP)"
      :args "{task-id}"})})

(def ^:private create-story-pr-action
  {:expr
   '(task-conductor.project.resolvers/invoke-skill!
     {:skill "mcp-tasks:create-story-pr (MCP)"
      :args "{task-id}"})})

(def ^:private merge-pr-action
  {:expr '(task-conductor.project.resolvers/invoke-skill!
           {:skill "squash-merge-on-gh"
            :args "{task-id}"})})

(def ^:private complete-story-action
  {:expr '(task-conductor.project.resolvers/invoke-skill!
           {:skill "complete-story"})})

(def ^:private escalate-action
  {:expr
   '(task-conductor.project.resolvers/escalate-to-dev-env!
     {})})

;;; Shared Statechart Elements

(def ^:private all-derived-task-states
  "All derived states for tasks (used as event keywords)."
  [:unrefined :refined :done :awaiting-pr :wait-pr-merge :complete :terminated])

(def ^:private all-derived-story-states
  "All derived states for stories (used as event keywords)."
  [:unrefined :refined :has-tasks :done
   :awaiting-pr :wait-pr-merge :complete :terminated])

(defn- re-derive-transitions
  "Build transitions from a state to all other derived states.
  Excludes the state's own keyword (no self-transition) and any
  events already handled by the state. This allows on-dev-env-close
  to transition from any non-terminal state after manual escalation."
  [own-state handled-events all-states]
  (let [exclude (conj (set handled-events) own-state)]
    (mapv (fn [s] (sc/transition {:event s :target s}))
          (remove exclude all-states))))

(defn- escalated-state
  "Build the :escalated compound state with sub-states.
  Includes :session-idle/:session-running sub-states.
  `resume-events` is a coll of event keywords for outgoing
  transitions that resume normal flow."
  [resume-events]
  (apply sc/state {:id :escalated :initial :session-idle}
         (sc/on-entry {}
                      (sc/action escalate-action))
         (sc/state {:id :session-idle}
                   (sc/transition
                    {:event :on-active
                     :target :session-running}))
         (sc/state {:id :session-running}
                   (sc/transition
                    {:event :on-session-idle
                     :target :session-idle}))
         (mapv (fn [evt] (sc/transition {:event evt :target evt}))
               resume-events)))

;;; Statechart Definitions

(def task-statechart
  "Statechart for standalone task execution.
   States correspond to derive-task-state return values.
   Transitions triggered by sending derived state as event.

   Flow: idle → unrefined → refined → done →
         awaiting-pr → wait-pr-merge → complete
   Any state can transition to :escalated on :error."
  (sc/statechart {:initial :idle}
    ;; Idle - waiting for initial state check after session start
                 (sc/state {:id :idle}
                           (sc/transition
                            {:event :unrefined :target :unrefined})
                           (sc/transition {:event :refined :target :refined})
                           (sc/transition {:event :done :target :done})
                           (sc/transition
                            {:event :awaiting-pr :target :awaiting-pr})
                           (sc/transition
                            {:event :wait-pr-merge :target :wait-pr-merge})
                           (sc/transition {:event :complete :target :complete}))

    ;; Unrefined - needs task refinement
                 (apply sc/state {:id :unrefined}
                        (sc/on-entry {}
                                     (sc/action refine-task-action))
                        (sc/transition {:event :refined :target :refined})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :unrefined [:refined :error :no-progress]
                         all-derived-task-states))

    ;; Refined - ready for execution
                 (apply sc/state {:id :refined}
                        (sc/on-entry {}
                                     (sc/action execute-task-action))
                        (sc/transition {:event :done :target :done})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :refined [:done :error :no-progress]
                         all-derived-task-states))

    ;; Done - executed, awaiting code review
                 (apply sc/state {:id :done}
                        (sc/on-entry {}
                                     (sc/action review-task-action))
                        (sc/transition
                         {:event :awaiting-pr :target :awaiting-pr})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :done [:awaiting-pr :error :no-progress]
                         all-derived-task-states))

    ;; Awaiting PR - needs PR creation
                 (apply sc/state {:id :awaiting-pr}
                        (sc/on-entry {}
                                     (sc/action create-task-pr-action))
                        (sc/transition
                         {:event :wait-pr-merge :target :wait-pr-merge})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :awaiting-pr [:wait-pr-merge :error :no-progress]
                         all-derived-task-states))

    ;; Wait for PR merge - awaiting merge notification
                 (apply sc/state {:id :wait-pr-merge}
      ;; No entry action - waiting for external event
                        (sc/transition
                         {:event :merge-pr :target :merging-pr})
                        (sc/transition {:event :complete :target :complete})
                        (sc/transition {:event :error :target :escalated})
                        (re-derive-transitions
                         :wait-pr-merge [:merge-pr :complete :error]
                         all-derived-task-states))

    ;; Merging PR - squash-merges the PR on GitHub
                 (apply sc/state {:id :merging-pr}
                        (sc/on-entry {}
                                     (sc/action merge-pr-action))
                        (sc/transition {:event :complete :target :complete})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :merging-pr [:complete :error :no-progress]
                         all-derived-task-states))

    ;; Complete - close task on mcp-tasks.
    ;; Re-derivation after /complete-story returns :complete when
    ;; the task is closed, so transition to :terminated on that.
                 (apply sc/state {:id :complete}
                        (sc/on-entry {}
                                     (sc/action complete-story-action))
                        (sc/transition
                         {:event :complete :target :terminated})
                        (sc/transition
                         {:event :terminated :target :terminated})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :complete [:complete :terminated :error :no-progress]
                         all-derived-task-states))

                 (sc/final {:id :terminated})

    ;; Escalated - error, human intervention needed
                 (escalated-state
                  [:unrefined :refined :done
                   :awaiting-pr :wait-pr-merge
                   :complete :terminated])))

(def story-statechart
  "Statechart for story execution.
   States correspond to derive-story-state return values.
   Transitions triggered by sending derived state as event.

   Flow: idle → unrefined → refined → has-tasks →
         done → awaiting-pr → wait-pr-merge → complete
   has-tasks can loop or return from done.
   Any state can transition to :escalated on :error."
  (sc/statechart {:initial :idle}
    ;; Idle - waiting for initial state check
                 (sc/state {:id :idle}
                           (sc/transition
                            {:event :unrefined :target :unrefined})
                           (sc/transition {:event :refined :target :refined})
                           (sc/transition
                            {:event :has-tasks :target :has-tasks})
                           (sc/transition {:event :done :target :done})
                           (sc/transition
                            {:event :awaiting-pr :target :awaiting-pr})
                           (sc/transition
                            {:event :wait-pr-merge :target :wait-pr-merge})
                           (sc/transition {:event :complete :target :complete}))

    ;; Unrefined - needs story refinement
                 (apply sc/state {:id :unrefined}
                        (sc/on-entry {}
                                     (sc/action refine-task-action))
                        (sc/transition {:event :refined :target :refined})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :unrefined [:refined :error :no-progress]
                         all-derived-story-states))

    ;; Refined - needs task creation
                 (apply sc/state {:id :refined}
                        (sc/on-entry {}
                                     (sc/action create-story-tasks-action))
                        (sc/transition
                         {:event :has-tasks :target :has-tasks})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :refined [:has-tasks :error :no-progress]
                         all-derived-story-states))

    ;; Has tasks - execute next incomplete child
    ;; State re-derivation after child completion may skip intermediate
    ;; states, so allow direct transitions to any downstream state.
                 (apply sc/state {:id :has-tasks}
                        (sc/on-entry {}
                                     (sc/action execute-story-child-action))
                        (sc/transition
                         {:event :has-tasks :target :has-tasks})
                        (sc/transition {:event :done :target :done})
                        (sc/transition
                         {:event :awaiting-pr :target :awaiting-pr})
                        (sc/transition
                         {:event :wait-pr-merge :target :wait-pr-merge})
                        (sc/transition
                         {:event :complete :target :complete})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :has-tasks [:has-tasks :done :awaiting-pr
                                     :wait-pr-merge :complete
                                     :error :no-progress]
                         all-derived-story-states))

    ;; Done - all children complete, review story.
    ;; Reviews *entire* story, not a single task.
                 (apply sc/state {:id :done}
                        (sc/on-entry {}
                                     (sc/action review-story-action))
      ;; Review may find issues requiring more work
                        (sc/transition
                         {:event :has-tasks :target :has-tasks})
                        (sc/transition
                         {:event :awaiting-pr :target :awaiting-pr})
                        (sc/transition
                         {:event :wait-pr-merge :target :wait-pr-merge})
                        (sc/transition
                         {:event :complete :target :complete})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :done [:has-tasks :awaiting-pr :wait-pr-merge
                                :complete :error :no-progress]
                         all-derived-story-states))

    ;; Awaiting PR - reviewed, needs PR creation
                 (apply sc/state {:id :awaiting-pr}
                        (sc/on-entry {}
                                     (sc/action create-story-pr-action))
                        (sc/transition
                         {:event :wait-pr-merge :target :wait-pr-merge})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :awaiting-pr [:wait-pr-merge :error :no-progress]
                         all-derived-story-states))

    ;; Wait for PR merge - awaiting merge notification
                 (apply sc/state {:id :wait-pr-merge}
      ;; No entry action - waiting for external event
                        (sc/transition
                         {:event :merge-pr :target :merging-pr})
                        (sc/transition {:event :complete :target :complete})
                        (sc/transition {:event :error :target :escalated})
                        (re-derive-transitions
                         :wait-pr-merge [:merge-pr :complete :error]
                         all-derived-story-states))

    ;; Merging PR - squash-merges the PR on GitHub
                 (apply sc/state {:id :merging-pr}
                        (sc/on-entry {}
                                     (sc/action merge-pr-action))
                        (sc/transition {:event :complete :target :complete})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :merging-pr [:complete :error :no-progress]
                         all-derived-story-states))

    ;; Complete - close story on mcp-tasks.
    ;; Re-derivation after /complete-story returns :complete when
    ;; the story is closed, so transition to :terminated on that.
                 (apply sc/state {:id :complete}
                        (sc/on-entry {}
                                     (sc/action complete-story-action))
                        (sc/transition
                         {:event :complete :target :terminated})
                        (sc/transition
                         {:event :terminated :target :terminated})
                        (sc/transition {:event :error :target :escalated})
                        (sc/transition
                         {:event :no-progress :target :escalated})
                        (re-derive-transitions
                         :complete [:complete :terminated :error :no-progress]
                         all-derived-story-states))

                 (sc/final {:id :terminated})

    ;; Escalated - error, human intervention needed
                 (escalated-state
                  [:unrefined :refined :has-tasks :done
                   :awaiting-pr :wait-pr-merge
                   :complete :terminated])))

;;; Statechart Registration
;; Register statecharts on namespace load for use by execute! mutation.

(defn register-statecharts!
  "Register task and story statecharts with the engine.
   Called automatically on namespace load."
  []
  (sc/register! :execute/task task-statechart)
  (sc/register! :execute/story story-statechart))

(register-statecharts!)
