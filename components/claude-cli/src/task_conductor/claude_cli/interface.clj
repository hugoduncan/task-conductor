(ns task-conductor.claude-cli.interface
  "Public interface for Claude CLI invocation."
  (:require [task-conductor.claude-cli.core :as core]))

(defn invoke
  "Invoke Claude CLI with the given options. Placeholder for implementation."
  [opts]
  (core/build-args opts)
  nil)
