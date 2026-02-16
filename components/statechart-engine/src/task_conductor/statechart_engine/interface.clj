(ns task-conductor.statechart-engine.interface
  "Public interface for the statechart engine component.
  All consumers should import this namespace rather than core.

  DSL re-exports from com.fulcrologic.statecharts for chart construction:
    statechart, state, transition, initial, final, parallel, history,
    on-entry, on-exit, assign, Send, action

  Engine functions for managing statecharts:
    register!, unregister!, start!, send!, stop!, reset-engine!

  Introspection functions:
    current-state, list-sessions, list-charts, available-events, history

  Actions can execute EQL via the pathom-graph component:
    - vector? expression → EQL query
    - fn? expression → direct call (escape hatch)
    - (symbol ...) expression → mutation"
  (:require
   [task-conductor.statechart-engine.core :as core]
   [task-conductor.statechart-engine.resolvers] ; auto-registers resolvers
   [com.fulcrologic.statecharts.chart :as chart]
   [com.fulcrologic.statecharts.elements :as ele]))

;;; DSL Re-exports for Chart Construction
;; These allow consumers to build statecharts
;; without importing fulcrologic namespaces.

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
  attrs keys: :event (keyword or list), :cond,
  :target (keyword or set), :type (:internal/:external)"
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
  Note: Named history-node to avoid conflict with
  introspection history function."
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
  "Send an event to a target
  (external system, this machine, or another machine).
  attrs keys: :event, :target, :delay, :content, etc.
  Note: Capitalized to avoid conflict with clojure.core/send."
  ele/Send)

(def action
  "Define an executable action with an expression.
  attrs keys: :expr - the expression to execute
  Expression types:
    - vector? → EQL query (e.g., [:user/name])
    - fn? → direct call with statechart env (escape hatch)
    - (symbol ...) → mutation (e.g., (my.ns/do-thing! {:arg 1}))"
  ele/script)

;;; Engine Functions

(def register!
  "Register a statechart definition under the given name.
  Returns chart-name on success.
  Throws if name already registered or chart-def invalid."
  core/register!)

(def unregister!
  "Remove a chart registration from the engine.
  This removes the chart from both the engine's local registry tracking and
  the underlying fulcrologic statecharts registry. Sessions already started
  with this chart will continue to function until stopped.
  Returns chart-name on success. Throws if not registered."
  core/unregister!)

(def start!
  "Start a new session of the registered chart.
  Options:
    :max-history-size - limit history entries (nil = unlimited, default)
  Returns session-id on success. Throws if chart not registered."
  core/start!)

(def send!
  "Send an event to a session.
  Returns state-config (set of active states) on success.
  Throws if session doesn't exist."
  core/send!)

(def stop!
  "Stop and remove a session.
  Returns session-id on success. Throws if session doesn't exist."
  core/stop!)

(def reset-engine!
  "Reset engine state. For testing only."
  core/reset-engine!)

;;; Introspection Functions

(defn current-state
  "Returns the current state configuration for a session.
  Returns state-config (set of active state keywords).
  Throws if session doesn't exist.
  Note: Named current-state to avoid conflict with ele/state."
  [session-id]
  (core/state session-id))

(def list-sessions
  "Returns vector of all active session IDs."
  core/list-sessions)

(def list-charts
  "Returns vector of all registered chart names."
  core/list-charts)

(def available-events
  "Returns events that would trigger transitions from the current state.
  Returns set of event keywords. Throws if session not found."
  core/available-events)

(def history
  "Returns the state transition history for a session.
  Returns [{:state config :event event :timestamp inst} ...]
  in chronological order.
  With optional n parameter, returns only the last n entries.
  Throws if session doesn't exist."
  core/history)

;;; Transition Listener Functions

(def add-transition-listener!
  "Register a callback for state transitions.
  Callback receives (session-id from-state to-state event).
  Returns listener-key."
  core/add-transition-listener!)

(def remove-transition-listener!
  "Remove a previously registered transition listener.
  Returns listener-key."
  core/remove-transition-listener!)

;;; Session Data Functions

(def get-data
  "Get the data model for a session.
  Returns the data map. Throws if session doesn't exist."
  core/get-data)

(def update-data!
  "Update the data model for a session.
  f is a function that takes the current data and returns new data.
  Returns the new data. Throws if session doesn't exist."
  core/update-data!)
