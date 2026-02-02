(ns task-conductor.statechart-engine.resolvers
  "EQL resolvers and mutations for engine operations.
   Registered on namespace load."
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.dev-env.interface :as dev-env]
   [task-conductor.dev-env.registry :as dev-env-registry]
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

(graph/defresolver engine-session-data [{:engine/keys [session-id]}]
  {::pco/input [:engine/session-id]
   ::pco/output [:engine/session-data]}
  {:engine/session-data (core/get-data session-id)})

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

;;; Dev-env Hook Registration

(defonce ^{:doc "Map of [dev-env-id hook-type] to {:hook-id :engine-session-id :event}."
           :private true}
  dev-env-hooks
  (atom {}))

(defn reset-dev-env-hooks!
  "Reset dev-env hooks registry. For testing only."
  []
  (reset! dev-env-hooks {}))

(graph/defmutation engine-register-dev-env-hook!
  "Register a dev-env hook that sends an event to a statechart session.
   When the dev-env fires the hook, the specified event is sent to the
   statechart session.

   Only one hook per (dev-env-id, hook-type) pair is tracked. Registering
   a second hook with the same pair overwrites the previous registration.
   The underlying dev-env callback from the first registration remains
   registered (DevEnv protocol has no unregister method), but the tracking
   in `dev-env-hooks` is replaced."
  [{:dev-env/keys [id hook-type] :engine/keys [session-id event]}]
  {::pco/output [:engine/hook-id]}
  (if-let [dev-env (dev-env-registry/get-dev-env id)]
    (let [callback (fn [_context]
                     (core/send! session-id event))
          ;; session-id is used both as the statechart session and dev-env session
          hook-id (dev-env/register-hook dev-env session-id hook-type callback)]
      (swap! dev-env-hooks assoc [id hook-type]
             {:hook-id hook-id
              :engine-session-id session-id
              :event event})
      {:engine/hook-id hook-id})
    {:engine/hook-id {:error :not-found :message (str "Dev-env not found: " id)}}))

(graph/defmutation engine-unregister-dev-env-hook!
  "Unregister a dev-env hook.
   Removes the hook from tracking. Note: the underlying dev-env callback
   remains registered as DevEnv protocol has no unregister method."
  [{:dev-env/keys [id hook-type]}]
  {::pco/output [:engine/unregistered?]}
  (let [key [id hook-type]
        existed? (contains? @dev-env-hooks key)]
    (swap! dev-env-hooks dissoc key)
    {:engine/unregistered? existed?}))

;;; Registration

(def all-operations
  "Vector of all engine resolvers and mutations."
  [engine-sessions
   engine-charts
   engine-session-state
   engine-session-history
   engine-session-data
   engine-start!
   engine-send!
   engine-stop!
   engine-register-dev-env-hook!
   engine-unregister-dev-env-hook!])

(defn register-resolvers!
  "Register all engine resolvers and mutations with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
