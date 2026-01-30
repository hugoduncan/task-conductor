(ns task-conductor.claude-cli.core
  "Core implementation for Claude CLI process management."
  (:require [babashka.process :as p]
            [clojure.data.json :as json]
            [clojure.string :as str])
  (:import [java.io BufferedReader InputStreamReader]
           [java.util.concurrent Executors TimeUnit ScheduledFuture]))

(defn build-args
  "Build CLI arguments vector from options map.
  Always includes --output-format stream-json, --verbose, and --print conversation-summary.
  Maps: :model, :allowed-tools, :disallowed-tools, :max-turns, :mcp-config.
  The :prompt value goes last without a flag."
  [opts]
  (let [{:keys [prompt model allowed-tools disallowed-tools max-turns mcp-config]} opts]
    (cond-> ["claude" "--output-format" "stream-json" "--verbose"
             "--print" "conversation-summary"]
      model (into ["--model" model])
      (seq allowed-tools) (into ["--allowedTools" (str/join "," allowed-tools)])
      (seq disallowed-tools) (into ["--disallowedTools" (str/join "," disallowed-tools)])
      max-turns (into ["--max-turns" (str max-turns)])
      mcp-config (into ["--mcp-config" mcp-config])
      prompt (conj prompt))))

(defn- parse-json-line
  "Parse a JSON line. Returns parsed map or parse-error map on failure."
  [line]
  (try
    (json/read-str line :key-fn keyword)
    (catch Exception _
      {:type "parse-error" :line line})))

(defn- read-stdout-lines
  "Read lines from process stdout, call callbacks, collect events.
  Returns vector of all events when stream ends."
  [^java.io.InputStream stdout on-line on-event]
  (let [reader (BufferedReader. (InputStreamReader. stdout))
        events (atom [])]
    (loop []
      (when-let [line (.readLine reader)]
        (when on-line (on-line line))
        (let [event (parse-json-line line)]
          (swap! events conj event)
          (when on-event (on-event event)))
        (recur)))
    @events))

(defn invoke-process
  "Invoke Claude CLI with the given options.
  Returns {:process p :result-promise promise}.

  Options:
    :prompt          - The prompt to send to Claude
    :dir             - Working directory for the process
    :timeout         - Timeout in milliseconds
    :on-line         - Callback for each raw line (fn [line] ...)
    :on-event        - Callback for each parsed event (fn [event] ...)
    Plus all options supported by build-args.

  Internal (for testing):
    :_args           - Override args vector (skips build-args)"
  [opts]
  (let [{:keys [dir timeout on-line on-event _args]} opts
        args (or _args (build-args opts))
        result-promise (promise)
        proc (p/process args (cond-> {:in "" :out :stream :err :inherit}
                               dir (assoc :dir dir)))
        ;; Schedule timeout if specified
        scheduler (when timeout (Executors/newSingleThreadScheduledExecutor))
        timeout-task (when scheduler
                       (.schedule scheduler
                                  ^Runnable (fn []
                                              (p/destroy proc)
                                              (deliver result-promise
                                                       {:exit-code nil :error :timeout})
                                              (.shutdown scheduler))
                                  ^long timeout
                                  TimeUnit/MILLISECONDS))]
    ;; Start reader thread
    (future
      (try
        (let [events (read-stdout-lines (:out proc) on-line on-event)
              exit-code (:exit @proc)]
          ;; Cancel timeout if it hasn't fired
          (when timeout-task
            (.cancel ^ScheduledFuture timeout-task false))
          (deliver result-promise {:exit-code exit-code :events events}))
        (catch Exception e
          ;; Process was likely destroyed by timeout or cancel
          (when-not (realized? result-promise)
            (deliver result-promise {:exit-code nil :error :interrupted :exception e})))
        (finally
          (when scheduler
            (.shutdown scheduler)))))
    {:process proc :result-promise result-promise}))

(defn cancel!
  "Cancel a running invocation by destroying the process.
  Returns true if the process was running and is now destroyed,
  false if already terminated."
  [handle]
  (let [p (:process handle)
        java-proc (:proc p)]
    (if (.isAlive java-proc)
      (do
        (p/destroy p)
        true)
      false)))
