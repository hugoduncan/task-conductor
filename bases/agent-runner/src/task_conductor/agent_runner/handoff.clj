(ns task-conductor.agent-runner.handoff
  "Handoff state management for SDK/CLI coordination.

   Provides file-based state persistence for coordinating handoffs between
   the Claude Agent SDK and Claude Code CLI. The handoff file acts as a
   shared communication channel."
  (:require
   [clojure.edn :as edn]
   [clojure.tools.logging :as log]
   [malli.core :as m]
   [malli.error :as me]
   [nextjournal.beholder :as beholder]
   [task-conductor.agent-runner.logging])
  (:import
   [java.io File FileNotFoundException]
   [java.nio.file Files StandardCopyOption]
   [java.time Instant]
   [java.time.format DateTimeFormatter]))

(def default-handoff-path ".task-conductor-handoff.edn")

(def HandoffState
  "Malli schema for handoff state between SDK and CLI (domain model)."
  [:map
   [:status [:enum :active :needs-input :completed :error]]
   [:session-id :string]
   [:task-id :int]
   [:story-id :int]
   [:timestamp inst?]
   [:handoff-reason {:optional true} :string]
   [:sdk-result {:optional true} :map]])

(def ^:private RawHandoffState
  "Malli schema for handoff state as stored in EDN file (string timestamp)."
  [:map
   [:status [:enum :active :needs-input :completed :error]]
   [:session-id :string]
   [:task-id :int]
   [:story-id :int]
   [:timestamp :string]
   [:handoff-reason {:optional true} :string]
   [:sdk-result {:optional true} :map]])

(defn- validate-state!
  "Validate state against HandoffState schema. Throws on invalid."
  [state]
  (when-not (m/validate HandoffState state)
    (throw (ex-info "Invalid handoff state"
                    {:type :validation-error
                     :errors (me/humanize (m/explain HandoffState state))})))
  state)

(defn- validate-raw-state!
  "Validate raw state (as read from file) against RawHandoffState schema."
  [state]
  (when-not (m/validate RawHandoffState state)
    (throw (ex-info "Invalid handoff state"
                    {:type :validation-error
                     :errors (me/humanize (m/explain RawHandoffState state))})))
  state)

(defn instant->iso8601
  "Convert java.time.Instant to ISO-8601 string."
  [^Instant inst]
  (.format DateTimeFormatter/ISO_INSTANT inst))

(defn iso8601->instant
  "Parse ISO-8601 string to java.time.Instant."
  [^String s]
  (Instant/parse s))

(defn- serialize-state
  "Convert state with Instant to EDN-writable form."
  [state]
  (update state :timestamp instant->iso8601))

(defn- deserialize-state
  "Convert EDN-read state back to domain form with Instant."
  [state]
  (update state :timestamp iso8601->instant))

(defn write-handoff-state
  "Write handoff state to file with atomic rename.

   Validates state against schema, writes to temp file, then atomically
   renames to target path. Throws on validation failure."
  ([state]
   (write-handoff-state state default-handoff-path))
  ([state path]
   (validate-state! state)
   (let [target-file (File. ^String path)
         parent-dir (.getParentFile target-file)
         temp-file (File/createTempFile "handoff" ".edn.tmp" parent-dir)
         serialized (serialize-state state)]
     (try
       (spit temp-file (pr-str serialized))
       (Files/move (.toPath temp-file)
                   (.toPath target-file)
                   (into-array [StandardCopyOption/ATOMIC_MOVE
                                StandardCopyOption/REPLACE_EXISTING]))
       (catch Exception e
         (.delete temp-file)
         (throw e))))
   state))

(defn read-handoff-state
  "Read handoff state from file.

   Reads and parses EDN, validates against schema, returns state with
   Instant. Throws FileNotFoundException if file missing, or ex-info
   on parse/validation errors."
  ([]
   (read-handoff-state default-handoff-path))
  ([path]
   (let [file (File. ^String path)]
     (when-not (.exists file)
       (throw (FileNotFoundException. (str "Handoff file not found: " path))))
     (let [content (slurp file)
           parsed (try
                    (edn/read-string content)
                    (catch Exception e
                      (throw (ex-info "Failed to parse handoff file"
                                      {:type :parse-error
                                       :path path}
                                      e))))]
       (validate-raw-state! parsed)
       (deserialize-state parsed)))))

(defn clear-handoff-state
  "Delete handoff file if it exists. No-op if file missing."
  ([]
   (clear-handoff-state default-handoff-path))
  ([path]
   (let [file (File. ^String path)]
     (when (.exists file)
       (.delete file)))
   nil))

(defn- file-not-found? [e]
  (instance? FileNotFoundException e))

(defn- parse-error? [e]
  (and (instance? clojure.lang.ExceptionInfo e)
       (= :parse-error (:type (ex-data e)))))

(def ^:private expected-watcher-exception?
  "Return true if exception is expected during file watching.
   FileNotFoundException and parse errors occur during atomic writes."
  (some-fn file-not-found? parse-error?))

(defn watch-handoff-file
  "Watch handoff file for changes and invoke callback with new state.

   Watches the parent directory and filters for the specific handoff file.
   On create or modify events, reads the state and invokes callback with
   the parsed state map. FileNotFoundException and parse errors are logged
   at DEBUG (expected during atomic writes). Other exceptions are logged
   at WARN to surface potential bugs.

   Returns a stop function that halts the watcher when called."
  ([callback]
   (watch-handoff-file callback default-handoff-path))
  ([callback path]
   (let [file (File. ^String path)
         filename (.getName file)
         parent-dir (or (.getParent file) ".")
         handler (fn [{:keys [type path] :as event}]
                   (when (and (#{:create :modify} type)
                              (= filename (str (.getFileName path))))
                     (try
                       (callback (read-handoff-state (.getAbsolutePath file)))
                       (catch Exception e
                         (if (expected-watcher-exception? e)
                           (log/debug e "Expected exception reading handoff file"
                                      {:event event})
                           (log/warn e "Unexpected exception reading handoff file"
                                     {:event event}))))))
         watcher (beholder/watch handler parent-dir)]
     (fn [] (beholder/stop watcher)))))
