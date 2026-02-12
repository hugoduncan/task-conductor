(ns task-conductor.dev-env.resolvers
  "EQL resolvers and mutations for generic dev-env operations.
   Uses the atom-based registry for dev-env discovery.
   Registered on namespace load."
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.dev-env.interface :as dev-env]
   [task-conductor.dev-env.registry :as registry]
   [task-conductor.pathom-graph.interface :as graph]))

;;; Resolvers

(graph/defresolver dev-env-list
  "List all registered dev-envs."
  []
  {::pco/output [:dev-env/available]}
  {:dev-env/available (registry/list-dev-envs)})

(graph/defresolver dev-env-selected
  "Select the first available dev-env from the registry."
  []
  {::pco/output [:dev-env/selected]}
  {:dev-env/selected (registry/select-dev-env)})

(graph/defresolver dev-env-by-id
  "Lookup a dev-env by ID with connection status."
  [{:dev-env/keys [id]}]
  {::pco/input [:dev-env/id]
   ::pco/output [:dev-env/instance :dev-env/type :dev-env/connected?]}
  (if-let [entry (registry/get-dev-env-entry id)]
    (let [dev-env (:dev-env entry)]
      {:dev-env/instance dev-env
       :dev-env/type (:type entry)
       :dev-env/connected? (dev-env/connected? dev-env)})
    {:dev-env/instance nil
     :dev-env/type nil
     :dev-env/connected? false}))

;;; Mutations

(graph/defmutation dev-env-start-session!
  "Start a Claude session in a dev-env.
   Returns session handle or error."
  [{:dev-env/keys [id session-id opts]}]
  {::pco/output [:dev-env/session-result]}
  (if-let [dev-env (registry/get-dev-env id)]
    {:dev-env/session-result (dev-env/start-session
                              dev-env
                              session-id
                              (or opts {}))}
    {:dev-env/session-result {:error :not-found
                              :message (str "Dev-env not found: " id)}}))

(graph/defmutation dev-env-close-session!
  "Close a Claude session in the dev-env."
  [{:dev-env/keys [id session-id]}]
  {::pco/output [:dev-env/close-result]}
  (if-let [dev-env (registry/get-dev-env id)]
    {:dev-env/close-result (dev-env/close-session dev-env session-id)}
    {:dev-env/close-result {:error :not-found
                            :message (str "Dev-env not found: " id)}}))

(graph/defmutation dev-env-query-transcript!
  "Query the transcript for a session."
  [{:dev-env/keys [id session-id]}]
  {::pco/output [:dev-env/transcript]}
  (if-let [dev-env (registry/get-dev-env id)]
    {:dev-env/transcript (dev-env/query-transcript dev-env session-id)}
    {:dev-env/transcript {:error :not-found
                          :message (str "Dev-env not found: " id)}}))

(graph/defmutation dev-env-query-events!
  "Query the events for a session."
  [{:dev-env/keys [id session-id]}]
  {::pco/output [:dev-env/events]}
  (if-let [dev-env (registry/get-dev-env id)]
    {:dev-env/events (dev-env/query-events dev-env session-id)}
    {:dev-env/events {:error :not-found
                      :message (str "Dev-env not found: " id)}}))

;;; Registration

(def all-operations
  "Vector of all dev-env resolvers and mutations."
  [dev-env-list
   dev-env-selected
   dev-env-by-id
   dev-env-start-session!
   dev-env-close-session!
   dev-env-query-transcript!
   dev-env-query-events!])

(defn register-resolvers!
  "Register all dev-env resolvers and mutations with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
