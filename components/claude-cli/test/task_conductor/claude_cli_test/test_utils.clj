(ns task-conductor.claude-cli-test.test-utils
  "Shared test utilities for claude-cli resolver tests."
  (:require
   [task-conductor.claude-cli.registry :as registry]
   [task-conductor.claude-cli.resolvers :as resolvers]
   [task-conductor.pathom-graph.interface :as graph]))

(defmacro with-clean-state
  "Execute body with clean registry and graph, cleaning up afterward."
  [& body]
  `(do
     (registry/clear-registry!)
     (graph/reset-graph!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         (registry/clear-registry!)
         (graph/reset-graph!)))))
