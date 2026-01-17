(ns task-conductor.agent-runner.cli
  "CLI integration for Claude Code session management.

   Provides functions to create Claude sessions via the CLI, which ensures
   sessions have Claude Code's system prompt, CLAUDE.md context, and MCP tools."
  (:require
   [babashka.process :as p]
   [clojure.data.json :as json]))

(def ^:private default-timeout-ms
  "Default timeout for CLI operations: 5 minutes."
  300000)

(defn create-session-via-cli
  "Create a new Claude session via the CLI.

   Spawns `claude --print --output-format json` to create a session with
   Claude Code's full context (system prompt, CLAUDE.md, MCP tools).

   Options:
   - :working-dir - directory to run claude from (required)
   - :prompt - initial prompt to send (required)
   - :timeout-ms - timeout in milliseconds (default: 120000)

   Returns map on success:
   - :session-id - the session identifier for resumption
   - :response - full parsed JSON response from CLI

   Throws ex-info on failure with :type key:
   - :cli/timeout - process exceeded timeout
   - :cli/non-zero-exit - process exited with non-zero code
   - :cli/parse-error - failed to parse JSON output
   - :cli/missing-session-id - response lacks session_id field"
  [{:keys [working-dir prompt timeout-ms]}]
  (let [timeout (or timeout-ms default-timeout-ms)
        cmd ["claude" "--print" "--output-format" "json" "-p" prompt]]
    (println "[create-session-via-cli] Launching CLI")
    (println "[create-session-via-cli]   working-dir:" working-dir)
    (println "[create-session-via-cli]   timeout-ms:" timeout)
    (println "[create-session-via-cli]   prompt:" (subs prompt 0 (min 100 (count prompt))))
    (println "[create-session-via-cli]   cmd:" (pr-str cmd))
    (let [proc (p/process {:dir working-dir
                           :in ""
                           :out :string
                           :err :string}
                          "claude" "--print" "--output-format" "json"
                          "-p" prompt)]
      (println "[create-session-via-cli] Process started, pid:" (.pid (:proc proc)))
      (println "[create-session-via-cli] Waiting for completion (timeout:" timeout "ms)...")
      (let [result (deref proc timeout ::timeout)]
        (println "[create-session-via-cli] Deref returned:" (if (= result ::timeout) "TIMEOUT" "completed"))
        (when (= result ::timeout)
          (println "[create-session-via-cli] TIMEOUT - destroying process tree")
          (p/destroy-tree proc)
          (throw (ex-info "CLI process timed out"
                          {:type :cli/timeout
                           :timeout-ms timeout
                           :working-dir working-dir})))
        (let [{:keys [exit out err]} result]
          (println "[create-session-via-cli] Exit code:" exit)
          (println "[create-session-via-cli] Stdout length:" (count out))
          (println "[create-session-via-cli] Stderr length:" (count err))
          (when (seq err)
            (println "[create-session-via-cli] Stderr:" (subs err 0 (min 500 (count err)))))
          (when-not (zero? exit)
            (throw (ex-info "CLI process exited with non-zero code"
                            {:type :cli/non-zero-exit
                             :exit-code exit
                             :stdout out
                             :stderr err
                             :working-dir working-dir})))
          (let [parsed (try
                         (json/read-str out :key-fn keyword)
                         (catch Exception e
                           (throw (ex-info "Failed to parse CLI JSON output"
                                           {:type :cli/parse-error
                                            :output out
                                            :parse-error (.getMessage e)
                                            :working-dir working-dir}))))]
            (println "[create-session-via-cli] Parsed response, session_id:" (:session_id parsed))
            (when-not (:session_id parsed)
              (throw (ex-info "CLI response missing session_id"
                              {:type :cli/missing-session-id
                               :response parsed
                               :working-dir working-dir})))
            {:session-id (:session_id parsed)
             :response parsed}))))))
