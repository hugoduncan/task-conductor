(ns task-conductor.agent-runner.events
  "Event model for SDK progress visibility.

   Captures and validates all SDK message types during story execution,
   enabling real-time monitoring and post-hoc analysis."
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [malli.core :as m]
   [malli.error :as me])
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
               (subvec new-buf (- (count new-buf) *max-buffer-size*))
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
  (str events-dir "/" session-id ".edn"))

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
