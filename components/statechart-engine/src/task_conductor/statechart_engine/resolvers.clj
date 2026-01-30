(ns task-conductor.statechart-engine.resolvers
  "EQL resolvers and mutations for engine operations.
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

;;; Mutations

(graph/defmutation engine-start! [{:engine/keys [chart-id]}]
  {::pco/output [:engine/session-id]}
  {:engine/session-id (core/start! chart-id)})

(graph/defmutation engine-send! [{:engine/keys [session-id event]}]
  {::pco/output [:engine/session-state]}
  {:engine/session-state (core/send! session-id event)})

(graph/defmutation engine-stop! [{:engine/keys [session-id]}]
  {::pco/output [:engine/session-id]}
  {:engine/session-id (core/stop! session-id)})

;;; Registration

(def all-operations
  "Vector of all engine resolvers and mutations."
  [engine-sessions
   engine-charts
   engine-session-state
   engine-session-history
   engine-start!
   engine-send!
   engine-stop!])

(defn register-resolvers!
  "Register all engine resolvers and mutations with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
