(ns task-conductor.emacs-dev-env.resolvers
  "DEPRECATED: Use task-conductor.dev-env.resolvers instead.

  This namespace provided Emacs-specific dev-env resolvers. With the addition
  of the generic dev-env registry and resolvers, this namespace is no longer
  needed. The generic resolvers in task-conductor.dev-env.resolvers work with
  any dev-env implementation registered in the generic registry.

  This namespace remains for backward compatibility but will be removed in
  a future release."
  {:deprecated "0.1.0"}
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.dev-env.interface :as dev-env]
   [task-conductor.emacs-dev-env.core :as core]
   [task-conductor.pathom-graph.interface :as graph]))

;;; Resolvers

(graph/defresolver available-dev-envs []
  {::pco/output [:dev-env/available]}
  {:dev-env/available (core/list-dev-envs)})

(graph/defresolver healthy-dev-envs []
  {::pco/output [:dev-env/healthy]}
  {:dev-env/healthy (core/list-healthy-dev-envs)})

(graph/defresolver selected-dev-env []
  {::pco/output [:dev-env/selected]}
  {:dev-env/selected (core/select-dev-env)})

(graph/defresolver dev-env-by-id [{:dev-env/keys [id]}]
  {::pco/input [:dev-env/id]
   ::pco/output [:dev-env/instance :dev-env/type :dev-env/connected?]}
  (if-let [dev-env (core/get-dev-env id)]
    {:dev-env/instance dev-env
     :dev-env/type :emacs
     :dev-env/connected? (core/connected? dev-env)}
    {:dev-env/instance nil
     :dev-env/type nil
     :dev-env/connected? false}))

(graph/defresolver dev-env-health [{:dev-env/keys [id]}]
  {::pco/input [:dev-env/id]
   ::pco/output [:dev-env/health]}
  {:dev-env/health (core/ping-by-id id)})

;;; Mutations

(graph/defmutation dev-env-start-session!
  "Start a Claude session in the selected dev-env.
   Returns session handle or error."
  [{:dev-env/keys [id session-id opts]}]
  {::pco/output [:dev-env/session-result]}
  (if-let [dev-env (core/get-dev-env id)]
    {:dev-env/session-result (dev-env/start-session dev-env session-id (or opts {}))}
    {:dev-env/session-result {:error :not-found :message (str "Dev-env not found: " id)}}))

(graph/defmutation dev-env-close-session!
  "Close a Claude session in the dev-env."
  [{:dev-env/keys [id session-id]}]
  {::pco/output [:dev-env/close-result]}
  (if-let [dev-env (core/get-dev-env id)]
    {:dev-env/close-result (dev-env/close-session dev-env session-id)}
    {:dev-env/close-result {:error :not-found :message (str "Dev-env not found: " id)}}))

(graph/defmutation dev-env-query-transcript!
  "Query the transcript for a session."
  [{:dev-env/keys [id session-id]}]
  {::pco/output [:dev-env/transcript-result]}
  (if-let [dev-env (core/get-dev-env id)]
    {:dev-env/transcript-result (dev-env/query-transcript dev-env session-id)}
    {:dev-env/transcript-result {:error :not-found :message (str "Dev-env not found: " id)}}))

;;; Registration

(def all-operations
  "Vector of all dev-env resolvers and mutations."
  [available-dev-envs
   healthy-dev-envs
   selected-dev-env
   dev-env-by-id
   dev-env-health
   dev-env-start-session!
   dev-env-close-session!
   dev-env-query-transcript!])

(defn register-resolvers!
  "Register all dev-env resolvers and mutations with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
