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
  "Execute body with fresh engine, graph, and dev-env state.
   Resets engine, Pathom graph, and dev-env registry, then registers resolvers."
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
