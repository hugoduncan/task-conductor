(ns task-conductor.claude-cli.resolvers
  "EQL mutations for Claude CLI invocation.
   Registered on namespace load."
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]
   [task-conductor.claude-cli.interface :as interface]
   [task-conductor.claude-cli.registry :as registry]
   [task-conductor.pathom-graph.interface :as graph]))

;;; Mutations

(graph/defmutation invoke!
  "Start an async Claude CLI invocation.
   Returns immediately with an invocation ID."
  [{:claude-cli/keys [prompt dir]}]
  {::pco/output [:claude-cli/invocation-id]}
  (let [opts (cond-> {:prompt prompt}
               dir (assoc :dir dir))
        handle (interface/invoke opts)
        id (registry/create-invocation! handle)]
    {:claude-cli/invocation-id id}))

(graph/defmutation cancel!
  "Cancel a running Claude CLI invocation.
   Returns the invocation ID, or error if not found."
  [{:claude-cli/keys [invocation-id]}]
  {::pco/output [:claude-cli/invocation-id :claude-cli/error]}
  (if-let [entry (registry/get-invocation invocation-id)]
    (do
      (interface/cancel! (:handle entry))
      (registry/update-invocation! invocation-id {:status :cancelled})
      {:claude-cli/invocation-id invocation-id})
    {:claude-cli/error :not-found
     :claude-cli/invocation-id invocation-id}))

;;; Resolvers

(graph/defresolver invocation-result
  "Resolve the status and result of a Claude CLI invocation.
   Returns pending status if not complete, full result when done.
   For unknown invocation-id, returns status :not-found with error."
  [{:claude-cli/keys [invocation-id]}]
  {::pco/input [:claude-cli/invocation-id]
   ::pco/output [:claude-cli/status
                 :claude-cli/exit-code
                 :claude-cli/events
                 :claude-cli/error]}
  (if-let [entry (registry/get-invocation invocation-id)]
    (let [{:keys [status handle]} entry]
      (if (= :cancelled status)
        {:claude-cli/status :cancelled}
        (let [result-promise (:result-promise handle)]
          (if (realized? result-promise)
            (let [result @result-promise
                  new-status (if (:error result) :error :complete)]
              (registry/update-invocation! invocation-id
                                           {:status new-status :result result})
              {:claude-cli/status new-status
               :claude-cli/exit-code (:exit-code result)
               :claude-cli/events (:events result)})
            {:claude-cli/status :pending}))))
    {:claude-cli/status :not-found
     :claude-cli/error :not-found}))

;;; Registration

(def all-operations
  "Vector of all claude-cli operations."
  [invoke!
   cancel!
   invocation-result])

(defn register-resolvers!
  "Register all claude-cli mutations with pathom-graph.
   Called automatically on namespace load. Can be called again after graph reset."
  []
  (graph/register! all-operations))

;; Register on namespace load
(register-resolvers!)
