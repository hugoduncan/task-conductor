(ns task-conductor.agent-runner.core
  "Bootstrap namespace for the agent-runner base.
   Requiring this namespace loads all component resolvers and statecharts
   via auto-registration on namespace load."
  (:require
   [task-conductor.claude-cli.resolvers]
   [task-conductor.dev-env.resolvers]
   [task-conductor.mcp-tasks.resolvers]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.execute]
   [task-conductor.project.resolvers :as resolvers]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.resolvers]))

(def ^:private resolver-namespaces
  "Namespaces loaded for resolver auto-registration."
  ['task-conductor.claude-cli.resolvers
   'task-conductor.dev-env.resolvers
   'task-conductor.mcp-tasks.resolvers
   'task-conductor.project.resolvers
   'task-conductor.statechart-engine.resolvers])

(defn bootstrap!
  "Log which namespaces were loaded and verify the graph is operational.
   Returns a map with :namespaces loaded and :graph-operational? status."
  []
  (let [operational? (try
                       (some? (graph/env))
                       (catch Exception _ false))]
    (println "[agent-runner] Bootstrap complete:"
             (count resolver-namespaces) "resolver namespaces,"
             "graph operational:" operational?)
    {:namespaces resolver-namespaces
     :graph-operational? operational?}))

(defn- execute-and-start!
  "Call execute! mutation via EQL, then send initial-state event to
   start the statechart. Returns {:session-id :state :error}."
  [project-dir task-id]
  (let [result (graph/query
                [`(resolvers/execute!
                   {:task/project-dir ~project-dir
                    :task/id ~task-id})])
        {:execute/keys [session-id initial-state error]}
        (get result `resolvers/execute!)]
    (if error
      (do (println "[agent-runner] execute! failed:"
                   (:error error) (:message error))
          {:session-id nil :state nil :error error})
      (let [state (sc/send! session-id initial-state)]
        (println "[agent-runner] Session" session-id
                 "started, state:" state)
        {:session-id session-id :state state :error nil}))))

(defn run-story!
  "Start supervised execution of a story.
   Returns {:session-id :state :error}."
  [project-dir story-id]
  (execute-and-start! project-dir story-id))

(defn run-task!
  "Start supervised execution of a standalone task.
   Returns {:session-id :state :error}."
  [project-dir task-id]
  (execute-and-start! project-dir task-id))
