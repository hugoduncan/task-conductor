(ns task-conductor.statechart-engine.test-helpers
  "Shared test utilities for statechart-engine tests."
  (:require
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
  "Execute body with fresh engine and graph state.
   Resets both engine and Pathom graph, then registers resolvers."
  [& body]
  `(do
     (core/reset-engine!)
     (graph/reset-graph!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         (core/reset-engine!)
         (graph/reset-graph!)))))
