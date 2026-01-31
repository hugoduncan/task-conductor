(ns task-conductor.dev-env.registry
  "Registry for managing multiple dev-env instances.

  Provides atom-based storage for dev-env implementations, enabling
  resolvers to discover and select available dev-envs at runtime.")

(defonce ^:private registry
  (atom {}))

(defn register!
  "Add a dev-env to the registry.

  Parameters:
    dev-env - A DevEnv protocol implementation
    type    - Keyword identifying the implementation type (e.g., :emacs, :terminal)
    meta    - Optional map of additional metadata

  Returns the generated dev-env ID (string)."
  ([dev-env type]
   (register! dev-env type {}))
  ([dev-env type meta]
   (let [id (str (gensym "dev-env-"))]
     (swap! registry assoc id {:dev-env dev-env
                               :type type
                               :meta meta})
     id)))

(defn unregister!
  "Remove a dev-env from the registry.

  Parameters:
    id - The dev-env ID returned from register!

  Returns true if the dev-env was removed, false if not found."
  [id]
  (let [existed? (contains? @registry id)]
    (swap! registry dissoc id)
    existed?))

(defn get-dev-env
  "Lookup a dev-env by ID.

  Parameters:
    id - The dev-env ID

  Returns the DevEnv instance, or nil if not found."
  [id]
  (get-in @registry [id :dev-env]))

(defn get-dev-env-entry
  "Lookup a dev-env entry by ID.

  Parameters:
    id - The dev-env ID

  Returns map with :dev-env, :type, :meta, or nil if not found."
  [id]
  (get @registry id))

(defn list-dev-envs
  "List all registered dev-envs.

  Returns a seq of maps with :id, :type, and :meta for each registered dev-env.
  The dev-env instances themselves are not included to avoid leaking impl details."
  []
  (mapv (fn [[id entry]]
          {:id id
           :type (:type entry)
           :meta (:meta entry)})
        @registry))

(defn select-dev-env
  "Select a dev-env from the registry.

  Returns the first available dev-env entry as {:id :dev-env :type :meta},
  or nil if registry is empty.

  Future implementations may add selection criteria (e.g., by type, health check)."
  []
  (when-let [[id entry] (first @registry)]
    {:id id
     :dev-env (:dev-env entry)
     :type (:type entry)
     :meta (:meta entry)}))

(defn clear!
  "Clear all entries from the registry. Primarily for testing."
  []
  (reset! registry {})
  nil)
