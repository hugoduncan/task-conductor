(ns task-conductor.claude-cli.core
  "Core implementation for Claude CLI process management."
  (:require [babashka.process :as p]
            [clojure.string :as str]))

(defn build-args
  "Build CLI arguments vector from options map.
  Always includes --output-format stream-json and --print conversation-summary.
  Maps: :model, :allowed-tools, :disallowed-tools, :max-turns, :mcp-config.
  The :prompt value goes last without a flag."
  [opts]
  (let [{:keys [prompt model allowed-tools disallowed-tools max-turns mcp-config]} opts]
    (cond-> ["claude" "--output-format" "stream-json"
             "--print" "conversation-summary"]
      model (into ["--model" model])
      (seq allowed-tools) (into ["--allowedTools" (str/join "," allowed-tools)])
      (seq disallowed-tools) (into ["--disallowedTools" (str/join "," disallowed-tools)])
      max-turns (into ["--max-turns" (str max-turns)])
      mcp-config (into ["--mcp-config" mcp-config])
      prompt (conj prompt))))

(defn invoke-process
  "Invoke a process with given args. Placeholder for implementation."
  [args opts]
  (p/process args opts))
