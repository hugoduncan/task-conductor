(ns task-conductor.project.work-on
  "State derivation functions for task and story execution.
   These pure functions derive execution state from mcp-tasks data,
   enabling statechart-driven automation.")

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
   - :awaiting-pr - executed, needs PR creation
   - :wait-pr-merge - PR created, awaiting merge
   - :complete    - task closed

   Task map should contain:
   - :status      - :open, :closed, :in-progress, :blocked
   - :meta        - map with :refined key when refined
   - :pr-num      - GitHub PR number when PR created
   - :pr-merged?  - true when PR has been merged (external check)"
  [task]
  (cond
    (= :closed (:status task))
    :complete

    (and (:pr-num task) (:pr-merged? task))
    :complete

    (:pr-num task)
    :wait-pr-merge

    (refined? task)
    :refined

    :else
    :unrefined))

;;; Story State Derivation

(defn- children-complete?
  "Check if all children are complete (status :closed)."
  [children]
  (and (seq children)
       (every? #(= :closed (:status %)) children)))

(defn- has-incomplete-children?
  "Check if there are any incomplete children."
  [children]
  (and (seq children)
       (some #(not= :closed (:status %)) children)))

(defn derive-story-state
  "Derive execution state for a story.

   Returns one of:
   - :unrefined       - needs refinement (:meta :refined is nil)
   - :refined         - refined but no children created yet
   - :has-tasks       - has incomplete child tasks to execute
   - :awaiting-review - all children complete, needs code review
   - :awaiting-pr     - reviewed, needs PR creation
   - :wait-pr-merge   - PR created, awaiting merge
   - :complete        - story closed

   Story map should contain:
   - :status        - :open, :closed, :in-progress, :blocked
   - :meta          - map with :refined key when refined
   - :code-reviewed - ISO-8601 timestamp when reviewed
   - :pr-num        - GitHub PR number when PR created
   - :pr-merged?    - true when PR has been merged (external check)

   Children is a seq of child task maps."
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
    :awaiting-review

    (has-incomplete-children? children)
    :has-tasks

    (refined? story)
    :refined

    :else
    :unrefined))
