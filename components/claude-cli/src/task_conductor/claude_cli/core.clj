(ns task-conductor.claude-cli.core
  "Core implementation for Claude CLI process management."
  (:require [babashka.process :as p]
            [clojure.data.json :as json]
            [clojure.string :as str])
  (:import [java.io BufferedReader InputStreamReader]
           [java.util.concurrent Executors TimeUnit ScheduledFuture]))

;;; Nullable Support

(def ^:dynamic *nullable*
  "When bound to a nullable config map, invoke-process returns configured
  responses instead of spawning real processes. Used for testing."
  nil)

(defn- nullable-invoke
  "Handle invocation when *nullable* is bound.
  Records the invocation and returns configured result."
  [nullable opts]
  (let [{:keys [invocations config]} nullable
        {:keys [exit-code events session-id error]} config
        result (if error
                 {:exit-code nil :error error}
                 {:exit-code (or exit-code 0)
                  :events (or events [])
                  :session-id (or session-id "test-session")})
        result-promise (doto (promise) (deliver result))]
    (swap! invocations conj {:opts opts :timestamp (java.time.Instant/now)})
    {:process nil :result-promise result-promise}))

(declare invoke-process*)

;;; CLI Argument Building

(defn build-args
  "Build CLI arguments vector from options map.
  Always includes --output-format stream-json, --verbose, and --print.
  Maps: :model, :allowed-tools, :disallowed-tools, :max-turns, :mcp-config,
  :hooks.
  The :prompt value goes last without a flag."
  [opts]
  (let [{:keys
         [prompt
          model
          allowed-tools
          disallowed-tools
          max-turns
          mcp-config
          hooks]} opts]
    (cond-> ["claude" "--output-format" "stream-json" "--verbose"
             "--print"]
      model (into ["--model" model])
      (seq allowed-tools) (into ["--allowedTools" (str/join "," allowed-tools)])
      (seq
       disallowed-tools) (into
                          ["--disallowedTools" (str/join "," disallowed-tools)])
      max-turns (into ["--max-turns" (str max-turns)])
      mcp-config (into ["--mcp-config" mcp-config])
      (seq hooks) (into ["--settings"
                         (json/write-str {:hooks hooks})])
      prompt (conj prompt))))

(defn- parse-json-line
  "Parse a JSON line. Returns parsed map or parse-error map on failure."
  [line]
  (try
    (json/read-str line :key-fn keyword)
    (catch Exception _
      {:type "parse-error" :line line})))

(defn extract-session-id
  "Extract session-id from events.
  Looks for the first event with a :session_id field.
  Returns nil if no session-id found."
  [events]
  (some :session_id events))

(defn- read-stdout-lines
  "Read lines from process stdout, call callbacks, collect events.
  Callback exceptions are caught and recorded as :callback-error events.
  Returns vector of all events when stream ends."
  [^java.io.InputStream stdout on-line on-event]
  (let [reader (BufferedReader. (InputStreamReader. stdout))
        events (atom [])]
    (loop []
      (when-let [line (.readLine reader)]
        (when on-line
          (try
            (on-line line)
            (catch Exception e
              (swap! events conj {:type "callback-error"
                                  :callback :on-line
                                  :error (.getMessage e)}))))
        (let [event (parse-json-line line)]
          (swap! events conj event)
          (when on-event
            (try
              (on-event event)
              (catch Exception e
                (swap! events conj {:type "callback-error"
                                    :callback :on-event
                                    :error (.getMessage e)})))))
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
    :_args           - Override args vector (skips build-args)

  When *nullable* is bound, returns configured response without subprocess."
  [opts]
  (if *nullable*
    (nullable-invoke *nullable* opts)
    (invoke-process* opts)))

(defn- invoke-process*
  "Real implementation of invoke-process with subprocess."
  [opts]
  (let [{:keys [dir timeout on-line on-event _args]} opts
        args (or _args (build-args opts))
        result-promise (promise)
        proc (p/process args (cond-> {:in "" :out :stream :err :inherit
                                      :extra-env {"CLAUDECODE" ""}}
                               dir (assoc :dir dir)))
        ;; Schedule timeout if specified
        scheduler (when timeout (Executors/newSingleThreadScheduledExecutor))
        timeout-task (when scheduler
                       (.schedule scheduler
                                  ^Runnable (fn []
                                              (p/destroy proc)
                                              (deliver result-promise
                                                       {:exit-code nil
                                                        :error :timeout})
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
          (deliver result-promise {:exit-code exit-code
                                   :events events
                                   :session-id (extract-session-id events)}))
        (catch Exception e
          ;; Process was likely destroyed by timeout or cancel
          (when-not (realized? result-promise)
            (deliver
             result-promise
             {:exit-code nil :error :interrupted :exception e})))
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
