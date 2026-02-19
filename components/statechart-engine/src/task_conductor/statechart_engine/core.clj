(ns task-conductor.statechart-engine.core
  "Core statechart engine implementation.
  Provides singleton environment for registering
  statecharts and managing sessions.
  Actions execute EQL through Pathom (pathom-graph component)."
  (:require
   [clojure.set :as set]
   [com.fulcrologic.statecharts :as sc]
   [com.fulcrologic.statecharts.chart :as chart]
   [com.fulcrologic.statecharts.protocols :as sp]
   [com.fulcrologic.statecharts.simple :as simple]
   [com.fulcrologic.statecharts.events :as evts]
   [task-conductor.pathom-graph.interface :as graph]))

;;; EQL Executor

(defn- inject-session-id
  "Inject :engine/session-id into mutation params.
  Handles mutations with or without params."
  [expression session-id]
  (let [mutation-sym (first expression)
        params (or (second expression) {})]
    (list mutation-sym (assoc params :engine/session-id session-id))))

(defrecord EQLExecutionModel []
  sp/ExecutionModel
  (run-expression! [_this env expression]
    ;; expression is the :expr from action/assign elements
    ;; env contains statechart context including data-model
    (when expression
      ;; Session-id is stored in working memory, not directly in env
      (let [vwmem (::sc/vwmem env)
            session-id (when vwmem (::sc/session-id @vwmem))]
        (cond
          ;; EQL vector - run as query
          (vector? expression)
          (graph/query expression)

          ;; Function - call with env and data (for assign compatibility)
          ;; Assign expressions expect (fn [env data] ...) signature
          (fn? expression)
          (let [data-model (::sc/data-model env)
                data       (sp/get-at data-model vwmem [])]
            (expression env data))

          ;; Symbol list (mutation call) - wrap and run
          ;; Inject session-id into params for context access
          ;; Use seq? to handle both lists and Cons (from syntax-quote)
          (and (seq? expression) (symbol? (first expression)))
          (graph/query [(inject-session-id expression session-id)])

          :else
          (throw (ex-info "Unknown action expression type"
                          {:expression expression
                           :type (type expression)})))))))

(defn- eql-env
  "Create statechart env with EQL execution model."
  []
  (simple/simple-env
   {::sc/execution-model (->EQLExecutionModel)}))

;;; Singleton State

(defonce ^{:doc "Singleton statecharts environment."} env
  (eql-env))

(defonce ^{:doc "Set of registered chart names."} charts
  (atom #{}))

(defonce ^{:doc "Map of session-id to working memory."} sessions
  (atom {}))

(defonce ^{:doc "Map of session-id to history entries.
  Each entry is {:state config :event event :timestamp inst}."} histories
  (atom {}))

(defonce
  ^{:doc "Map of session-id to max history size (nil = unlimited)."}
  history-limits
  (atom {}))

(defonce
  ^{:doc "Map of session-id to lock object for serializing sends."}
  session-locks
  (atom {}))

(defonce
  ^{:doc "Map of listener-key to callback fn.
  Callbacks receive (session-id from-state to-state event)."}
  transition-listeners
  (atom {}))

;;; Internal Helpers

(defn- generate-session-id []
  (str (java.util.UUID/randomUUID)))

(defn- notify-listeners!
  "Notify all transition listeners of a state change."
  [session-id from-state to-state event]
  (doseq [[key f] @transition-listeners]
    (try
      (f session-id from-state to-state event)
      (catch Exception e
        (binding [*out* *err*]
          (println "WARN: transition listener" key
                   "threw:" (.getMessage e)))))))

(defonce ^{:doc "Map of session-id to data model values."} session-data
  (atom {}))

(defn- valid-chart-def?
  "Returns true if chart-def is a valid statechart definition."
  [chart-def]
  (and (map? chart-def)
       (= :statechart (:node-type chart-def))))

;;; Public API

(defn register!
  "Register a statechart definition under the given name.
  Returns chart-name on success.
  Throws if name already registered or chart-def is invalid."
  [chart-name chart-def]
  (cond
    (not (valid-chart-def? chart-def))
    (throw (ex-info "Invalid chart definition"
                    {:error :invalid-chart-def :chart-name chart-name}))

    (contains? @charts chart-name)
    (throw (ex-info "Chart already registered"
                    {:error :already-registered :chart-name chart-name}))

    :else
    (do
      (simple/register! env chart-name chart-def)
      (swap! charts conj chart-name)
      chart-name)))

(defn unregister!
  "Remove a chart registration from the engine.

  This removes the chart from both the engine's local registry tracking and
  the underlying fulcrologic statecharts registry. Sessions already started
  with this chart will continue to function until stopped.

  Returns chart-name on success. Throws if not registered."
  [chart-name]
  (if (contains? @charts chart-name)
    (let [registry (::sc/statechart-registry env)]
      ;; Remove from fulcrologic registry's internal atom
      (swap! (:charts registry) dissoc chart-name)
      ;; Remove from our tracking set
      (swap! charts disj chart-name)
      chart-name)
    (throw (ex-info "Chart not found"
                    {:error :not-found :chart-name chart-name}))))

(defn start!
  "Start a new session of the registered chart.
  Options:
    :max-history-size - limit history entries (nil = unlimited, default)
    :data             - initial data model map (stored for introspection)
  Returns session-id on success. Throws if chart not registered."
  ([chart-name] (start! chart-name nil))
  ([chart-name opts]
   (when-not (contains? @charts chart-name)
     (throw (ex-info "Chart not found"
                     {:error :chart-not-found :chart-name chart-name})))
   (let [session-id (generate-session-id)
         processor  (::sc/processor env)
         wmem       (sp/start! processor env chart-name
                               {::sc/session-id session-id})
         init-state (::sc/configuration wmem)
         init-entry {:state init-state
                     :event nil
                     :timestamp (java.time.Instant/now)}
         max-size   (:max-history-size opts)
         init-data  (assoc (:data opts) :session-id session-id)]
     (swap! sessions assoc session-id wmem)
     (swap! histories assoc session-id [init-entry])
     (swap! session-data assoc session-id init-data)
     (swap! session-locks assoc session-id (Object.))
     (when max-size
       (swap! history-limits assoc session-id max-size))
     session-id)))

(defn- trim-history
  "Trim history to max-size, keeping most recent entries."
  [entries max-size]
  (if (and max-size (> (count entries) max-size))
    (vec (take-last max-size entries))
    entries))

(defn send!
  "Send an event to a session.
  Returns state-config (set of active states) on success.
  Throws if session doesn't exist.
  Thread-safe: concurrent sends to the same session are serialized."
  [session-id event]
  (if-let [lock (get @session-locks session-id)]
    (locking lock
      (if-let [wmem (get @sessions session-id)]
        (let [from-state (::sc/configuration wmem)
              processor  (::sc/processor env)
              new-wmem   (sp/process-event!
                          processor
                          env
                          wmem
                          (evts/new-event event))
              new-state  (::sc/configuration new-wmem)
              new-entry  {:state new-state
                          :event event
                          :timestamp (java.time.Instant/now)}
              max-size   (get @history-limits session-id)]
          (swap! sessions assoc session-id new-wmem)
          (swap! histories update session-id
                 (fn [entries]
                   (trim-history (conj entries new-entry) max-size)))
          (notify-listeners! session-id from-state new-state event)
          new-state)
        ;; Session was stopped while we held the lock
        (throw (ex-info "Session not found"
                        {:error :session-not-found :session-id session-id}))))
    (throw (ex-info "Session not found"
                    {:error :session-not-found :session-id session-id}))))

(defn stop!
  "Stop and remove a session.
  Returns session-id on success. Throws if session doesn't exist."
  [session-id]
  (if (contains? @sessions session-id)
    (do
      (swap! sessions dissoc session-id)
      (swap! histories dissoc session-id)
      (swap! history-limits dissoc session-id)
      (swap! session-data dissoc session-id)
      (swap! session-locks dissoc session-id)
      session-id)
    (throw (ex-info "Session not found"
                    {:error :session-not-found :session-id session-id}))))

(defn reset-engine!
  "Reset engine state. For testing only."
  []
  (reset! charts #{})
  (reset! sessions {})
  (reset! histories {})
  (reset! history-limits {})
  (reset! session-data {})
  (reset! session-locks {})
  (reset! transition-listeners {}))

;;; Transition Listener API

(defn add-transition-listener!
  "Register a callback for state transitions.
  Callback receives (session-id from-state to-state event).
  Returns listener-key."
  [listener-key callback]
  (swap! transition-listeners assoc listener-key callback)
  listener-key)

(defn remove-transition-listener!
  "Remove a previously registered transition listener.
  Returns listener-key."
  [listener-key]
  (swap! transition-listeners dissoc listener-key)
  listener-key)

;;; Introspection API

(defn state
  "Returns the current state configuration for a session.
  Returns state-config (set of active state keywords).
  Throws if session doesn't exist."
  [session-id]
  (if-let [wmem (get @sessions session-id)]
    (::sc/configuration wmem)
    (throw (ex-info "Session not found"
                    {:error :session-not-found :session-id session-id}))))

(defn list-sessions
  "Returns vector of all active session IDs."
  []
  (vec (keys @sessions)))

(defn list-charts
  "Returns vector of all registered chart names."
  []
  (vec @charts))

(defn- get-statechart
  "Retrieves the statechart definition from the registry."
  [chart-name]
  (sp/get-statechart (::sc/statechart-registry env) chart-name))

(defn- events-from-transitions
  "Extracts event names from transition elements."
  [statechart transition-ids]
  (->> transition-ids
       (map #(chart/element statechart %))
       (keep :event)
       (mapcat #(if (coll? %) % [%]))
       (into #{})))

(defn- events-for-state
  "Returns events defined on transitions from the given state."
  [statechart state-id]
  (let [transition-ids (chart/transitions statechart state-id)]
    (events-from-transitions statechart transition-ids)))

(defn available-events
  "Returns events that would trigger transitions from the current state.
  Returns set of event keywords. Throws if session not found."
  [session-id]
  (if-let [wmem (get @sessions session-id)]
    (let [chart-name  (::sc/statechart-src wmem)
          statechart  (get-statechart chart-name)
          config      (::sc/configuration wmem)
          all-events  (reduce (fn [events state-id]
                                (into
                                 events
                                 (events-for-state statechart state-id)))
                              #{}
                              config)]
      all-events)
    (throw (ex-info "Session not found"
                    {:error :session-not-found :session-id session-id}))))

(defn history
  "Returns the state transition history for a session.
  Returns [{:state config :event event :timestamp inst} ...]
  in chronological order.
  With optional n parameter, returns only the last n entries.
  Throws if session doesn't exist."
  ([session-id]
   (if-let [entries (get @histories session-id)]
     entries
     (throw (ex-info "Session not found"
                     {:error :session-not-found :session-id session-id}))))
  ([session-id n]
   (if-let [entries (get @histories session-id)]
     (vec (take-last n entries))
     (throw (ex-info "Session not found"
                     {:error :session-not-found :session-id session-id})))))

;;; Session Data API

(defn get-data
  "Get the data model for a session.
  Returns the data map. Throws if session doesn't exist."
  [session-id]
  (if (contains? @sessions session-id)
    (get @session-data session-id)
    (throw (ex-info "Session not found"
                    {:error :session-not-found :session-id session-id}))))

(defn update-data!
  "Update the data model for a session.
  f is a function that takes the current data and returns new data.
  Returns the new data. Throws if session doesn't exist."
  [session-id f]
  (if (contains? @sessions session-id)
    (get (swap! session-data update session-id f) session-id)
    (throw (ex-info "Session not found"
                    {:error :session-not-found :session-id session-id}))))

;;; Session Query API

(defn query-sessions
  "Query active sessions filtered by state.
  Returns vec of maps with :session-id, :state, :task-id, :task-title,
  and :entered-state-at for sessions whose current state intersects
  the given state-filter set.

  Each session returns a single :state — the filtered states are expected
  to be mutually exclusive leaf states (e.g. :escalated, :idle).
  If multiple states match, the lexicographically first is returned.

  state-filter - set of state keywords to match (e.g. #{:escalated :idle})"
  [state-filter]
  (let [session-ids (keys @sessions)]
    (into []
          (keep
           (fn [sid]
             (let [current (state sid)
                   matching (set/intersection current state-filter)]
               (when (seq matching)
                 (let [data (get @session-data sid)
                       hist (get @histories sid)
                       last-entry (peek hist)]
                   {:session-id sid
                    :state (first (sort matching))
                    :task-id (:task-id data)
                    :task-title (:task-title data)
                    :entered-state-at (:timestamp last-entry)})))))
          session-ids)))
