(ns task-conductor.statechart-engine.core
  "Core statechart engine implementation.
  Provides singleton environment for registering statecharts and managing sessions."
  (:require
   [com.fulcrologic.statecharts :as sc]
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
                                {::sc/session-id session-id})]
      (swap! sessions assoc session-id wmem)
      {:ok session-id})))

(defn send!
  "Send an event to a session.
  Returns {:ok state-config} on success where state-config is the set of active states,
  or {:error :session-not-found} if session doesn't exist."
  [session-id event]
  (if-let [wmem (get @sessions session-id)]
    (let [processor (::sc/processor env)
          new-wmem  (sp/process-event! processor env wmem (evts/new-event event))]
      (swap! sessions assoc session-id new-wmem)
      {:ok (::sc/configuration new-wmem)})
    {:error :session-not-found}))

(defn stop!
  "Stop and remove a session.
  Returns {:ok session-id} on success, {:error :session-not-found} if session doesn't exist."
  [session-id]
  (if (contains? @sessions session-id)
    (do
      (swap! sessions dissoc session-id)
      {:ok session-id})
    {:error :session-not-found}))

(defn reset-engine!
  "Reset engine state. For testing only."
  []
  (reset! charts #{})
  (reset! sessions {}))
