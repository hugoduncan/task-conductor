(ns task-conductor.dev-env.test-helpers
  "Shared test utilities for dev-env component tests."
  (:require
   [task-conductor.dev-env.registry :as registry]
   [task-conductor.dev-env.resolvers :as resolvers]
   [task-conductor.pathom-graph.interface :as graph]))

(defmacro with-clean-dev-env-state
  "Execute body with clean dev-env registry and graph.
   Resets registry and graph, then registers dev-env resolvers.
   Cleans up afterwards."
  [& body]
  `(do
     (registry/clear!)
     (graph/reset-graph!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         (registry/clear!)
         (graph/reset-graph!)))))
