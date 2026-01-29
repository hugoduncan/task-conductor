(ns task-conductor.agent-runner.events
  "Event model for SDK progress visibility.

   Captures and validates all SDK message types during story execution,
   enabling real-time monitoring and post-hoc analysis."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.tools.logging :as log]
   [malli.core :as m]
   [malli.error :as me]
   [task-conductor.agent-runner.logging]
   [task-conductor.claude-agent-sdk.interface :as sdk])
  (:import
   [java.io File PushbackReader]
   [java.nio.file Files StandardCopyOption]
   [java.time Instant]
   [java.time.format DateTimeFormatter]))

;;; Event Types

(def EventType
  "Supported event types corresponding to SDK message types.
   - :text-block - Text output from the agent
   - :thinking-block - Agent thinking/reasoning
   - :tool-use-block - Tool invocation by the agent
   - :tool-result-block - Result from tool execution
   - :result-message - Final result message"
  [:enum :text-block :thinking-block :tool-use-block
   :tool-result-block :result-message])

;;; Event Schema

(def Event
  "Schema for SDK events.

   Required fields:
   - :timestamp - java.time.Instant when event occurred
   - :session-id - UUID string identifying the SDK session
   - :story-id - Integer story task ID
   - :type - EventType keyword

   Optional fields:
   - :task-id - Integer task ID (nil for story-level events)
   - :content - Event payload (type-dependent)"
  [:map
   [:timestamp inst?]
   [:session-id [:string {:min 1}]]
   [:story-id :int]
   [:type EventType]
   [:task-id {:optional true} [:maybe :int]]
   [:content {:optional true} :any]])

;;; Validation Functions

(defn valid-event?
  "Returns true if the event map is valid against Event schema."
  [event]
  (m/validate Event event))

(defn explain-event
  "Returns a humanized error map for validation failures, or nil if valid."
  [event]
  (when-let [explanation (m/explain Event event)]
    (me/humanize explanation)))

(defn validate-event!
  "Validate event against Event schema. Throws on invalid.
   Returns the event unchanged if valid."
  [event]
  (when-let [errors (explain-event event)]
    (throw (ex-info (str "Invalid Event: " (pr-str errors))
                    {:type :validation-error
                     :errors errors
                     :event event})))
  event)

;;; Event Creation

(defn create-event
  "Create a validated event with automatic timestamp.

   Arguments:
   - session-id: UUID string for the SDK session
   - story-id: Integer story task ID
   - event-type: One of EventType keywords
   - content: Event payload (optional)
   - opts: Optional map with :task-id

   Returns a validated event map.

   Throws ex-info if resulting event fails validation."
  ([session-id story-id event-type]
   (create-event session-id story-id event-type nil {}))
  ([session-id story-id event-type content]
   (create-event session-id story-id event-type content {}))
  ([session-id story-id event-type content opts]
   (let [event (cond-> {:timestamp (java.time.Instant/now)
                        :session-id session-id
                        :story-id story-id
                        :type event-type}
                 (some? (:task-id opts))
                 (assoc :task-id (:task-id opts))

                 (some? content)
                 (assoc :content content))]
     (validate-event! event))))

;;; Ring Buffer Storage

(def ^:dynamic *max-buffer-size*
  "Maximum number of events to keep in the in-memory buffer.
   When exceeded, oldest events are dropped."
  1000)

(defonce ^{:private true
           :doc "In-memory ring buffer for events. Stores a vector of events."}
  event-buffer
  (atom []))

(defn add-event!
  "Add a validated event to the buffer.
   Drops oldest events when buffer exceeds *max-buffer-size*.
   Returns the added event.

   Throws ex-info if event fails validation."
  [event]
  (validate-event! event)
  (swap! event-buffer
         (fn [buf]
           (let [new-buf (conj buf event)]
             (if (> (count new-buf) *max-buffer-size*)
               ;; vec forces a copy, preventing subvec from retaining
               ;; a reference to the original (ever-growing) vector
               (vec (subvec new-buf (- (count new-buf) *max-buffer-size*)))
               new-buf))))
  event)

(defn get-events
  "Retrieve events from the buffer.
   Optional filter map supports:
   - :type - filter by event type keyword
   - :session-id - filter by session ID string
   - :story-id - filter by story ID integer

   Returns a vector of matching events."
  ([]
   @event-buffer)
  ([filters]
   (let [events @event-buffer]
     (if (empty? filters)
       events
       (filterv (fn [event]
                  (and (if-let [t (:type filters)]
                         (= t (:type event))
                         true)
                       (if-let [s (:session-id filters)]
                         (= s (:session-id event))
                         true)
                       (if-let [st (:story-id filters)]
                         (= st (:story-id event))
                         true)))
                events)))))

(defn clear-events!
  "Clear all events from the buffer. Returns nil."
  []
  (reset! event-buffer [])
  nil)

;;; Timestamp Serialization

(defn instant->iso8601
  "Convert java.time.Instant to ISO-8601 string."
  [^Instant inst]
  (.format DateTimeFormatter/ISO_INSTANT inst))

(defn iso8601->instant
  "Parse ISO-8601 string to java.time.Instant."
  [^String s]
  (Instant/parse s))

(defn- serialize-event
  "Convert event with Instant timestamp to EDN-writable form."
  [event]
  (update event :timestamp instant->iso8601))

(defn- deserialize-event
  "Convert EDN-read event back to domain form with Instant timestamp."
  [event]
  (update event :timestamp iso8601->instant))

;;; Persistent File Storage

(def default-events-dir
  "Default directory for event files, relative to project root."
  ".task-conductor/events")

(defn- validate-events-dir!
  "Validate that events-dir is a valid directory path.
   Throws ex-info with clear error message for:
   - nil or empty string paths
   - paths that point to existing files (not directories)
   Returns the path if valid."
  [events-dir]
  (when (or (nil? events-dir) (and (string? events-dir) (empty? events-dir)))
    (throw (ex-info "events-dir must be a non-empty string"
                    {:type :invalid-events-dir
                     :events-dir events-dir})))
  (when-not (string? events-dir)
    (throw (ex-info "events-dir must be a string"
                    {:type :invalid-events-dir
                     :events-dir events-dir
                     :actual-type (type events-dir)})))
  (let [dir (File. ^String events-dir)]
    (when (and (.exists dir) (not (.isDirectory dir)))
      (throw (ex-info "events-dir path exists but is not a directory"
                      {:type :invalid-events-dir
                       :events-dir events-dir}))))
  events-dir)

(defn- ensure-dir!
  "Create directory if it doesn't exist. Returns the directory File."
  [^String path]
  (let [dir (File. path)]
    (when-not (.exists dir)
      (.mkdirs dir))
    dir))

(defn- session-file-path
  "Returns the file path for a session's events file."
  [events-dir session-id]
  (.getPath (io/file events-dir (str session-id ".edn"))))

(defn flush-events!
  "Write events for a session to persistent storage.

   Filters current buffer for the given session-id and writes all matching
   events to `<events-dir>/<session-id>.edn`. Each event is written
   as a separate EDN form on its own line.

   Uses atomic write (temp file + rename) to prevent partial writes.

   Options:
   - :events-dir - directory for event files (default: .task-conductor/events)

   Returns the number of events written."
  ([session-id]
   (flush-events! session-id {}))
  ([session-id opts]
   (let [events-dir (or (:events-dir opts) default-events-dir)]
     (validate-events-dir! events-dir)
     (ensure-dir! events-dir)
     (let [session-events (get-events {:session-id session-id})
           target-file (File. ^String (session-file-path events-dir session-id))
           temp-file (File/createTempFile "events" ".edn.tmp" (File. events-dir))]
       (try
         (with-open [w (io/writer temp-file)]
           (doseq [event session-events]
             (.write w (pr-str (serialize-event event)))
             (.write w "\n")))
         (Files/move (.toPath temp-file)
                     (.toPath target-file)
                     (into-array [StandardCopyOption/ATOMIC_MOVE
                                  StandardCopyOption/REPLACE_EXISTING]))
         (count session-events)
         (catch Exception e
           (.delete temp-file)
           (throw e)))))))

(defn load-session-events
  "Load events from persistent storage for a session.

   Reads events from `<events-dir>/<session-id>.edn`.
   Returns a vector of validated event maps with Instant timestamps.

   Options:
   - :events-dir - directory for event files (default: .task-conductor/events)

   Returns nil if the file doesn't exist.
   Throws ex-info on parse errors."
  ([session-id]
   (load-session-events session-id {}))
  ([session-id opts]
   (let [events-dir (or (:events-dir opts) default-events-dir)
         _ (validate-events-dir! events-dir)
         file (File. ^String (session-file-path events-dir session-id))]
     (when (.exists file)
       (with-open [rdr (PushbackReader. (io/reader file))]
         (loop [events []]
           (let [form (try
                        (edn/read {:eof ::eof} rdr)
                        (catch Exception e
                          (throw (ex-info "Failed to parse events file"
                                          {:type :parse-error
                                           :session-id session-id
                                           :path (str file)}
                                          e))))]
             (if (= ::eof form)
               events
               (let [event (deserialize-event form)]
                 (validate-event! event)
                 (recur (conj events event)))))))))))

;;; SDK Event Callback Integration

(def ^:private capturable-block-types
  "Set of content block types that should be captured as events."
  #{:text-block :thinking-block :tool-use-block :tool-result-block})

(defn- capture-content-blocks!
  "Capture content blocks from an AssistantMessage as individual events."
  [content session-id story-id task-id]
  (when (sequential? content)
    (doseq [block content]
      (when (capturable-block-types (:type block))
        (add-event! (create-event session-id story-id (:type block) block
                                  {:task-id task-id}))))))

(defn- process-sdk-message!
  "Process a parsed SDK message and capture relevant events.
   Updates context-atom with session-id when ResultMessage arrives.
   Returns the session-id if found in a ResultMessage, nil otherwise."
  [msg context-atom]
  (let [{:keys [session-id story-id task-id]} @context-atom]
    (case (:type msg)
      :assistant-message
      (do
        (capture-content-blocks! (:content msg) session-id story-id task-id)
        nil)

      :result-message
      (let [real-session-id (:session-id msg)]
        ;; Update context with real session-id for subsequent events
        (when real-session-id
          (swap! context-atom assoc :session-id real-session-id))
        ;; Capture the result message event with the real session-id
        (add-event! (create-event (or real-session-id session-id)
                                  story-id
                                  :result-message
                                  (dissoc msg :type)
                                  {:task-id task-id}))
        real-session-id)

      ;; Skip other message types (UserMessage, SystemMessage, StreamEvent)
      nil)))

(defn make-event-callback
  "Create an event callback function for SDK event capture.

   The callback parses raw Python messages and captures events to the
   in-memory buffer. It handles session-id discovery from ResultMessage.

   Arguments:
   - context: Map with :story-id (required) and optional :task-id
   - opts: Optional map with:
     - :session-id - Initial session-id (defaults to random UUID)

   Returns a map with:
   - :callback - Function to pass to SDK :event-callback option
   - :context-atom - Atom containing context, updated with session-id

   Example:
     (let [{:keys [callback context-atom]} (make-event-callback {:story-id 123})]
       (sdk/create-client {:event-callback callback})
       ;; After query completes:
       (:session-id @context-atom)) ;; => real session-id"
  ([context]
   (make-event-callback context {}))
  ([context opts]
   (let [initial-session-id (or (:session-id opts)
                                (str (java.util.UUID/randomUUID)))
         context-atom (atom (assoc context :session-id initial-session-id))
         callback (fn [py-msg]
                    (try
                      (let [parsed (sdk/parse-message py-msg)]
                        (process-sdk-message! parsed context-atom))
                      (catch Exception e
                        ;; Log but don't propagate - callback errors shouldn't
                        ;; break the SDK flow
                        (log/warn e "Error processing SDK message"
                                  {:story-id (:story-id @context-atom)
                                   :session-id (:session-id @context-atom)}))))]
     {:callback callback
      :context-atom context-atom})))

;;; Event Watch Support

(defn add-event-watch!
  "Add a watch function to the event buffer.

   The watch-fn receives (key atom old-state new-state) as with add-watch.
   Returns the watch key for use with remove-event-watch!."
  [key watch-fn]
  (add-watch event-buffer key watch-fn)
  key)

(defn remove-event-watch!
  "Remove a watch function from the event buffer.
   Returns the event-buffer atom."
  [key]
  (remove-watch event-buffer key))

;;; Event Formatting

(def ^:private type-colors
  "ANSI color codes for event types."
  {:text-block        "\u001b[32m"   ; green
   :thinking-block    "\u001b[36m"   ; cyan
   :tool-use-block    "\u001b[33m"   ; yellow
   :tool-result-block "\u001b[34m"   ; blue
   :result-message    "\u001b[35m"}) ; magenta

(def ^:private ansi-reset "\u001b[0m")

(defn- truncate-string
  "Truncate string to max-len chars, adding ... if truncated."
  [s max-len]
  (if (and s (> (count s) max-len))
    (str (subs s 0 (- max-len 3)) "...")
    s))

(defn- extract-content-preview
  "Extract a preview string from event content."
  [event]
  (let [content (:content event)]
    (cond
      (nil? content) ""
      (string? content) content
      (map? content)
      (or (:text content)
          (:name content)
          (when-let [c (:content content)]
            (if (string? c) c (pr-str c)))
          (pr-str content))
      :else (pr-str content))))

(defn- format-timestamp
  "Format timestamp as HH:mm:ss."
  [^Instant inst]
  (let [formatter (java.time.format.DateTimeFormatter/ofPattern "HH:mm:ss")
        zoned (.atZone inst (java.time.ZoneId/systemDefault))]
    (.format formatter zoned)))

(defn format-event-line
  "Format an event as a single readable line.

   Options:
   - :color? - Use ANSI colors (default true)
   - :max-content-len - Max content preview length (default 80)

   Returns a formatted string."
  ([event]
   (format-event-line event {}))
  ([event opts]
   (let [color? (get opts :color? true)
         max-len (get opts :max-content-len 80)
         ts (format-timestamp (:timestamp event))
         event-type (:type event)
         type-str (name event-type)
         preview (truncate-string (extract-content-preview event) max-len)
         color (when color? (type-colors event-type))
         reset (when color? ansi-reset)]
     (str ts " " color "[" type-str "]" reset " " preview))))
