(ns task-conductor.agent-runner.events
  "Event model for SDK progress visibility.

   Captures and validates all SDK message types during story execution,
   enabling real-time monitoring and post-hoc analysis."
  (:require
   [malli.core :as m]
   [malli.error :as me]))

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
