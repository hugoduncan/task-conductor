(ns task-conductor.statechart-engine.core
  "Core statechart engine implementation.
  Provides singleton environment for registering statecharts and managing sessions."
  (:require
   [com.fulcrologic.statecharts :as sc]
   [com.fulcrologic.statecharts.chart :as chart]
   [com.fulcrologic.statecharts.protocols :as sp]
   [com.fulcrologic.statecharts.simple :as simple]
   [com.fulcrologic.statecharts.events :as evts]))

;;; Singleton State

(defonce ^{:doc "Singleton statecharts environment."} env
  (simple/simple-env))

(defonce ^{:doc "Set of registered chart names."} charts
  (atom #{}))

(defonce ^{:doc "Map of session-id to working memory."} sessions
  (atom {}))

(defonce ^{:doc "Map of session-id to history entries.
  Each entry is {:state config :event event :timestamp inst}."} histories
  (atom {}))

;;; Internal Helpers

(defn- generate-session-id []
  (str (java.util.UUID/randomUUID)))

;;; Public API

(defn register!
  "Register a statechart definition under the given name.
  Returns {:ok chart-name} on success, {:error :already-registered} if name exists."
  [chart-name chart-def]
  (if (contains? @charts chart-name)
    {:error :already-registered}
    (do
      (simple/register! env chart-name chart-def)
      (swap! charts conj chart-name)
      {:ok chart-name})))

(defn unregister!
  "Remove a chart registration.
  Returns {:ok chart-name} on success, {:error :not-found} if not registered."
  [chart-name]
  (if (contains? @charts chart-name)
    (do
      (swap! charts disj chart-name)
      {:ok chart-name})
    {:error :not-found}))

(defn start!
  "Start a new session of the registered chart.
  Returns {:ok session-id} on success, {:error :chart-not-found} if chart not registered."
  [chart-name]
  (if-not (contains? @charts chart-name)
    {:error :chart-not-found}
    (let [session-id (generate-session-id)
          processor  (::sc/processor env)
          wmem       (sp/start! processor env chart-name
                                {::sc/session-id session-id})
          init-state (::sc/configuration wmem)
          init-entry {:state init-state :event nil :timestamp (java.time.Instant/now)}]
      (swap! sessions assoc session-id wmem)
      (swap! histories assoc session-id [init-entry])
      {:ok session-id})))

(defn send!
  "Send an event to a session.
  Returns {:ok state-config} on success where state-config is the set of active states,
  or {:error :session-not-found} if session doesn't exist."
  [session-id event]
  (if-let [wmem (get @sessions session-id)]
    (let [processor  (::sc/processor env)
          new-wmem   (sp/process-event! processor env wmem (evts/new-event event))
          new-state  (::sc/configuration new-wmem)
          new-entry  {:state new-state :event event :timestamp (java.time.Instant/now)}]
      (swap! sessions assoc session-id new-wmem)
      (swap! histories update session-id conj new-entry)
      {:ok new-state})
    {:error :session-not-found}))

(defn stop!
  "Stop and remove a session.
  Returns {:ok session-id} on success, {:error :session-not-found} if session doesn't exist."
  [session-id]
  (if (contains? @sessions session-id)
    (do
      (swap! sessions dissoc session-id)
      (swap! histories dissoc session-id)
      {:ok session-id})
    {:error :session-not-found}))

(defn reset-engine!
  "Reset engine state. For testing only."
  []
  (reset! charts #{})
  (reset! sessions {})
  (reset! histories {}))

;;; Introspection API

(defn state
  "Returns the current state configuration for a session.
  Returns {:ok state-config} where state-config is a set of active state keywords,
  or {:error :session-not-found} if session doesn't exist."
  [session-id]
  (if-let [wmem (get @sessions session-id)]
    {:ok (::sc/configuration wmem)}
    {:error :session-not-found}))

(defn list-sessions
  "Returns {:ok [session-id ...]} with all active session IDs."
  []
  {:ok (vec (keys @sessions))})

(defn list-charts
  "Returns {:ok [chart-name ...]} with all registered chart names."
  []
  {:ok (vec @charts)})

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
  Returns {:ok #{event ...}} or {:error :session-not-found}."
  [session-id]
  (if-let [wmem (get @sessions session-id)]
    (let [chart-name  (::sc/statechart-src wmem)
          statechart  (get-statechart chart-name)
          config      (::sc/configuration wmem)
          all-events  (reduce (fn [events state-id]
                                (into events (events-for-state statechart state-id)))
                              #{}
                              config)]
      {:ok all-events})
    {:error :session-not-found}))

(defn history
  "Returns the state transition history for a session.
  Returns {:ok [{:state config :event event :timestamp inst} ...]} in chronological order,
  or {:error :session-not-found} if session doesn't exist.
  With optional n parameter, returns only the last n entries."
  ([session-id]
   (if-let [entries (get @histories session-id)]
     {:ok entries}
     {:error :session-not-found}))
  ([session-id n]
   (if-let [entries (get @histories session-id)]
     {:ok (vec (take-last n entries))}
     {:error :session-not-found})))
