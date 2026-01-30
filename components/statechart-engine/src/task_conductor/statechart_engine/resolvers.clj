(ns task-conductor.statechart-engine.resolvers
  "EQL resolvers for engine introspection.
   Registered on namespace load."
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.statechart-engine.core :as core]))

;;; Resolvers

(graph/defresolver engine-sessions []
  {::pco/output [:engine/sessions]}
  {:engine/sessions (core/list-sessions)})

(graph/defresolver engine-charts []
  {::pco/output [:engine/charts]}
  {:engine/charts (core/list-charts)})

(graph/defresolver engine-session-state [{:engine/keys [session-id]}]
  {::pco/input [:engine/session-id]
   ::pco/output [:engine/session-state]}
  {:engine/session-state (core/state session-id)})

(graph/defresolver engine-session-history [{:engine/keys [session-id]}]
  {::pco/input [:engine/session-id]
   ::pco/output [:engine/session-history]}
  {:engine/session-history (core/history session-id)})

;;; Registration

(def all-resolvers
  "Vector of all engine introspection resolvers."
  [engine-sessions
   engine-charts
   engine-session-state
   engine-session-history])

(defn register-resolvers!
  "Register all engine introspection resolvers with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-resolvers))

;; Register on namespace load
(register-resolvers!)
