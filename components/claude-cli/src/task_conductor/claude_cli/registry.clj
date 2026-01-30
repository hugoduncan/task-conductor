(ns task-conductor.claude-cli.registry
  "Invocation registry for tracking async Claude CLI invocations.
  Maps invocation IDs to their handles, status, and results.")

(defonce ^:private invocations
  (atom {}))

(defn clear-registry!
  "Clear all invocations from the registry. For testing only."
  []
  (reset! invocations {}))

(defn create-invocation!
  "Create a new invocation entry with a generated UUID.
  Returns the UUID. Entry is initialized with :status :pending and :result nil."
  [handle]
  (let [id (random-uuid)]
    (swap! invocations assoc id {:handle handle
                                 :status :pending
                                 :result nil})
    id))

(defn get-invocation
  "Look up an invocation by ID.
  Returns the entry map or nil if not found."
  [id]
  (get @invocations id))

(defn update-invocation!
  "Update fields in an invocation entry.
  Takes an ID and a map of fields to merge into the entry.
  Returns the updated entry, or nil if ID not found."
  [id updates]
  (let [result (atom nil)]
    (swap! invocations
           (fn [m]
             (if (contains? m id)
               (let [updated (update m id merge updates)]
                 (reset! result (get updated id))
                 updated)
               m)))
    @result))

(defn remove-invocation!
  "Remove an invocation entry from the registry.
  Returns the removed entry, or nil if ID not found."
  [id]
  (let [result (atom nil)]
    (swap! invocations
           (fn [m]
             (if-let [entry (get m id)]
               (do
                 (reset! result entry)
                 (dissoc m id))
               m)))
    @result))
