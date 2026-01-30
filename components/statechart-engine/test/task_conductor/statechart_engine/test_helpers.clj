(ns task-conductor.statechart-engine.test-helpers
  "Shared test utilities for statechart-engine tests."
  (:require
   [task-conductor.statechart-engine.core :as core]))

(defmacro with-clean-engine
  "Execute body with a fresh engine state, resetting before and after."
  [& body]
  `(do
     (core/reset-engine!)
     (try
       ~@body
       (finally
         (core/reset-engine!)))))
