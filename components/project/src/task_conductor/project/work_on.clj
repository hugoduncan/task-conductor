(ns task-conductor.project.work-on
  "State derivation and statechart definitions for task/story execution.
   Pure functions derive execution state from mcp-tasks data.
   Statecharts orchestrate state-driven automation with skill invocation."
  (:require
   [task-conductor.statechart-engine.interface :as sc]))

;;; Task State Derivation

(defn refined?
  "Check if task/story has been refined.
   Refined status is stored in :meta map with :refined key."
  [task]
  (some? (get-in task [:meta :refined])))

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

(defn count-open-children
  "Count the number of open (non-closed, non-deleted) children.
   Used for detecting no-progress in :has-tasks state."
  [children]
  (count (filter #(not (#{:closed "closed"} (:status %)))
                 (active-children children))))

(defn- children-complete?
  "Check if all active children are complete (status :done, :closed, or string equivalents)."
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
;; sending the derived state keyword as an event (e.g., :refined, :has-tasks).
;; Entry actions invoke skills via work-on mutations (implemented in later tasks).

;; State sets serve as reference documentation for valid statechart states.
;; Verified by tests to match the actual statechart definitions.

(def task-states
  "Valid states for standalone task execution."
  #{:idle :unrefined :refined :done :awaiting-pr :wait-pr-merge :complete :escalated})

(def story-states
  "Valid states for story execution."
  #{:idle :unrefined :refined :has-tasks :done
    :awaiting-pr :wait-pr-merge :complete :escalated})

(def task-statechart
  "Statechart for standalone task execution.
   States correspond to derive-task-state return values.
   Transitions are triggered by sending the derived state as an event.

   Flow: idle → unrefined → refined → done → awaiting-pr → wait-pr-merge → complete
   Any state can transition to :escalated on :error event."
  (sc/statechart {:initial :idle}
    ;; Idle - waiting for initial state check after session start
                 (sc/state {:id :idle}
                           (sc/transition {:event :unrefined :target :unrefined})
                           (sc/transition {:event :refined :target :refined})
                           (sc/transition {:event :done :target :done})
                           (sc/transition {:event :awaiting-pr :target :awaiting-pr})
                           (sc/transition {:event :wait-pr-merge :target :wait-pr-merge})
                           (sc/transition {:event :complete :target :complete}))

    ;; Unrefined - needs task refinement
                 (sc/state {:id :unrefined}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/invoke-skill!
                                                            {:skill "mcp-tasks:refine-task"})}))
                           (sc/transition {:event :refined :target :refined})
                           (sc/transition {:event :error :target :escalated})
                           (sc/transition {:event :no-progress :target :escalated}))

    ;; Refined - ready for execution
                 (sc/state {:id :refined}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/invoke-skill!
                                                            {:skill "mcp-tasks:execute-task"})}))
                           (sc/transition {:event :done :target :done})
                           (sc/transition {:event :error :target :escalated})
                           (sc/transition {:event :no-progress :target :escalated}))

    ;; Done - executed, awaiting code review
                 (sc/state {:id :done}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/invoke-skill!
                                                            {:skill "mcp-tasks:review-task"})}))
                           (sc/transition {:event :awaiting-pr :target :awaiting-pr})
                           (sc/transition {:event :error :target :escalated})
                           (sc/transition {:event :no-progress :target :escalated}))

    ;; Awaiting PR - task executed, needs PR creation
                 (sc/state {:id :awaiting-pr}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/invoke-skill!
                                                            {:skill "mcp-tasks:create-task-pr"})}))
                           (sc/transition {:event :wait-pr-merge :target :wait-pr-merge})
                           (sc/transition {:event :error :target :escalated})
                           (sc/transition {:event :no-progress :target :escalated}))

    ;; Wait for PR merge - PR created, awaiting merge notification from dev-env
                 (sc/state {:id :wait-pr-merge}
      ;; No entry action - waiting for external PR merge event
                           (sc/transition {:event :complete :target :complete})
                           (sc/transition {:event :error :target :escalated}))

    ;; Complete - task finished
                 (sc/final {:id :complete})

    ;; Escalated - error occurred, notify dev-env for human intervention
                 (sc/state {:id :escalated}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/escalate-to-dev-env! {})})))))

(def story-statechart
  "Statechart for story execution.
   States correspond to derive-story-state return values.
   Transitions are triggered by sending the derived state as an event.

   Flow: idle → unrefined → refined → has-tasks → done →
         awaiting-pr → wait-pr-merge → complete
   Note: has-tasks can loop (multiple children) or return from done.
   Any state can transition to :escalated on :error event."
  (sc/statechart {:initial :idle}
    ;; Idle - waiting for initial state check after session start
                 (sc/state {:id :idle}
                           (sc/transition {:event :unrefined :target :unrefined})
                           (sc/transition {:event :refined :target :refined})
                           (sc/transition {:event :has-tasks :target :has-tasks})
                           (sc/transition {:event :done :target :done})
                           (sc/transition {:event :awaiting-pr :target :awaiting-pr})
                           (sc/transition {:event :wait-pr-merge :target :wait-pr-merge})
                           (sc/transition {:event :complete :target :complete}))

    ;; Unrefined - needs story refinement
                 (sc/state {:id :unrefined}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/invoke-skill!
                                                            {:skill "mcp-tasks:refine-task"})}))
                           (sc/transition {:event :refined :target :refined})
                           (sc/transition {:event :error :target :escalated})
                           (sc/transition {:event :no-progress :target :escalated}))

    ;; Refined - needs task creation
                 (sc/state {:id :refined}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/invoke-skill!
                                                            {:skill "mcp-tasks:create-story-tasks"})}))
                           (sc/transition {:event :has-tasks :target :has-tasks})
                           (sc/transition {:event :error :target :escalated})
                           (sc/transition {:event :no-progress :target :escalated}))

    ;; Has tasks - execute next incomplete child task
                 (sc/state {:id :has-tasks}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/invoke-skill!
                                                            {:skill "mcp-tasks:execute-story-child"})}))
      ;; Can stay in has-tasks (more children) or move to done
                           (sc/transition {:event :has-tasks :target :has-tasks})
                           (sc/transition {:event :done :target :done})
                           (sc/transition {:event :error :target :escalated})
                           (sc/transition {:event :no-progress :target :escalated}))

    ;; Done - all children complete, awaiting code review
                 (sc/state {:id :done}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/invoke-skill!
                                                            {:skill "mcp-tasks:review-story-implementation"})}))
      ;; Review may find issues requiring more work
                           (sc/transition {:event :has-tasks :target :has-tasks})
                           (sc/transition {:event :awaiting-pr :target :awaiting-pr})
                           (sc/transition {:event :error :target :escalated})
                           (sc/transition {:event :no-progress :target :escalated}))

    ;; Awaiting PR - reviewed, needs PR creation
                 (sc/state {:id :awaiting-pr}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/invoke-skill!
                                                            {:skill "mcp-tasks:create-story-pr"})}))
                           (sc/transition {:event :wait-pr-merge :target :wait-pr-merge})
                           (sc/transition {:event :error :target :escalated})
                           (sc/transition {:event :no-progress :target :escalated}))

    ;; Wait for PR merge - PR created, awaiting merge notification from dev-env
                 (sc/state {:id :wait-pr-merge}
      ;; No entry action - waiting for external PR merge event
                           (sc/transition {:event :complete :target :complete})
                           (sc/transition {:event :error :target :escalated}))

    ;; Complete - story finished
                 (sc/final {:id :complete})

    ;; Escalated - error occurred, notify dev-env for human intervention
                 (sc/state {:id :escalated}
                           (sc/on-entry {}
                                        (sc/action {:expr '(task-conductor.project.resolvers/escalate-to-dev-env! {})})))))

;;; Statechart Registration
;; Register statecharts on namespace load for use by work-on! mutation.

(defn register-statecharts!
  "Register task and story statecharts with the engine.
   Called automatically on namespace load."
  []
  (sc/register! :work-on/task task-statechart)
  (sc/register! :work-on/story story-statechart))

(register-statecharts!)
