(ns task-conductor.claude-cli.core
  "Core implementation for Claude CLI process management."
  (:require [babashka.process :as p]))

(defn build-args
  "Build CLI arguments from options map. Placeholder for implementation."
  [_opts]
  [])

(defn invoke-process
  "Invoke a process with given args. Placeholder for implementation."
  [args opts]
  (p/process args opts))
