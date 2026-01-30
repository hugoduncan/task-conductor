(ns task-conductor.statechart-engine.interface
  "Public interface for the statechart engine component.
  All consumers should import this namespace rather than core.

  DSL re-exports from com.fulcrologic.statecharts for chart construction:
    statechart, state, transition, initial, final, parallel, history,
    on-entry, on-exit, assign, Send

  Engine functions for managing statecharts:
    register!, unregister!, start!, send!, stop!, reset-engine!

  Introspection functions:
    current-state, list-sessions, list-charts, available-events, history"
  (:require
   [task-conductor.statechart-engine.core :as core]
   [com.fulcrologic.statecharts.chart :as chart]
   [com.fulcrologic.statecharts.elements :as ele]))

;;; DSL Re-exports for Chart Construction
;; These allow consumers to build statecharts without importing fulcrologic namespaces.

(def statechart
  "Create a statechart definition.
  attrs keys: :initial (initial state id), :name, :binding
  children: state, parallel, or initial elements."
  chart/statechart)

(def state
  "Create a state node.
  attrs keys: :id, :initial, :initial?
  children: transition, state, on-entry, on-exit elements."
  ele/state)

(def transition
  "Define a transition between states.
  attrs keys: :event (keyword or list), :cond, :target (keyword or set), :type (:internal/:external)"
  ele/transition)

(def initial
  "Alias for (state {:initial? true} ...).
  Creates an initial state within a compound state."
  ele/initial)

(def final
  "Create a final state node.
  attrs keys: :id"
  ele/final)

(def parallel
  "Create a parallel state node for orthogonal regions.
  attrs keys: :id
  children: state elements for each parallel region."
  ele/parallel)

(def history-node
  "Create a history pseudo-state.
  attrs keys: :id, :type (:deep or :shallow), :deep? (alias for type)
  default-transition: transition element or target keyword.
  Note: Named history-node to avoid conflict with introspection history function."
  ele/history)

(def on-entry
  "Define entry actions for a state.
  children: executable content (assign, Send, script, etc.)"
  ele/on-entry)

(def on-exit
  "Define exit actions for a state.
  children: executable content (assign, Send, script, etc.)"
  ele/on-exit)

(def assign
  "Assign a value to the data model.
  attrs keys: :location (vector of keywords), :expr (value or fn)"
  ele/assign)

(def Send
  "Send an event to a target (external system, this machine, or another machine).
  attrs keys: :event, :target, :delay, :content, etc.
  Note: Capitalized to avoid conflict with clojure.core/send."
  ele/Send)

;;; Engine Functions

(def register!
  "Register a statechart definition under the given name.
  Returns {:ok chart-name} on success, {:error :already-registered} if name exists."
  core/register!)

(def unregister!
  "Remove a chart registration from the engine.
  This removes the chart from both the engine's local registry tracking and
  the underlying fulcrologic statecharts registry. Sessions already started
  with this chart will continue to function until stopped.
  Returns {:ok chart-name} on success, {:error :not-found} if not registered."
  core/unregister!)

(def start!
  "Start a new session of the registered chart.
  Returns {:ok session-id} on success, {:error :chart-not-found} if chart not registered."
  core/start!)

(def send!
  "Send an event to a session.
  Returns {:ok state-config} on success where state-config is the set of active states,
  or {:error :session-not-found} if session doesn't exist."
  core/send!)

(def stop!
  "Stop and remove a session.
  Returns {:ok session-id} on success, {:error :session-not-found} if session doesn't exist."
  core/stop!)

(def reset-engine!
  "Reset engine state. For testing only."
  core/reset-engine!)

;;; Introspection Functions

(defn current-state
  "Returns the current state configuration for a session.
  Returns {:ok state-config} where state-config is a set of active state keywords,
  or {:error :session-not-found} if session doesn't exist.
  Note: Named current-state to avoid conflict with ele/state."
  [session-id]
  (core/state session-id))

(def list-sessions
  "Returns {:ok [session-id ...]} with all active session IDs."
  core/list-sessions)

(def list-charts
  "Returns {:ok [chart-name ...]} with all registered chart names."
  core/list-charts)

(def available-events
  "Returns events that would trigger transitions from the current state.
  Returns {:ok #{event ...}} or {:error :session-not-found}."
  core/available-events)

(def history
  "Returns the state transition history for a session.
  Returns {:ok [{:state config :event event :timestamp inst} ...]} in chronological order,
  or {:error :session-not-found} if session doesn't exist.
  With optional n parameter, returns only the last n entries."
  core/history)
