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
   [task-conductor.project.resolvers]
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
