(ns task-conductor.statechart-engine.test-helpers
  "Shared test utilities for statechart-engine tests."
  (:require
   [task-conductor.dev-env.registry :as dev-env-registry]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.statechart-engine.core :as core]
   [task-conductor.statechart-engine.resolvers :as resolvers]))

(defmacro with-clean-engine
  "Execute body with a fresh engine state, resetting before and after."
  [& body]
  `(do
     (core/reset-engine!)
     (try
       ~@body
       (finally
         (core/reset-engine!)))))

(defmacro with-clean-state
  "Execute body with fresh engine, graph, dev-env registry, and hooks.
   Resets engine, Pathom graph, dev-env registry, and dev-env hooks,
   then registers resolvers. Cleans up afterwards.

   Note: This extends the basic dev-env cleanup with statechart-engine
   specific state (engine and dev-env hooks)."
  [& body]
  `(do
     (core/reset-engine!)
     (graph/reset-graph!)
     (dev-env-registry/clear!)
     (resolvers/reset-dev-env-hooks!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         (core/reset-engine!)
         (graph/reset-graph!)
         (dev-env-registry/clear!)
         (resolvers/reset-dev-env-hooks!)))))
