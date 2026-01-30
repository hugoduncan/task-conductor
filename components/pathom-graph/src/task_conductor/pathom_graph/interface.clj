(ns task-conductor.pathom-graph.interface
  "Public interface for the pathom-graph component.

   Core functions:
     register! - Register resolvers/mutations
     env       - Get current Pathom environment
     query     - Execute EQL queries

   Operation macros (re-exported from Pathom3):
     defresolver - Define a resolver
     defmutation - Define a mutation"
  (:require
   [task-conductor.pathom-graph.core :as core])
  (:require
   [com.wsscode.pathom3.connect.operation :as pco]))

;;; Core Functions

(def register!
  "Register resolver(s) or mutation(s). Accepts single op or collection.
   Replaces existing ops with same name. Invalidates env cache.
   Returns :registered."
  core/register!)

(def env
  "Returns current Pathom environment from registered operations.
   Caches result until registry changes."
  core/env)

(def query
  "Execute EQL query against the current environment.
   Returns result map. Optionally accepts entity data for parameterized queries.

   (query [:user/name])
   (query {:user/id 1} [:user/name])"
  core/query)

(def reset-graph!
  "Clear all registered operations. For testing only."
  core/reset-graph!)

;;; Operation Macros

(defmacro defresolver
  "Define a Pathom3 resolver. Re-exported from pathom3.

   (defresolver user-by-id [{:keys [user/id]}]
     {::pco/output [:user/name :user/email]}
     {:user/name \"Alice\" :user/email \"alice@example.com\"})"
  [& args]
  `(pco/defresolver ~@args))

(defmacro defmutation
  "Define a Pathom3 mutation. Re-exported from pathom3.

   (defmutation create-user! [{:keys [user/name]}]
     {::pco/output [:user/id]}
     {:user/id (random-uuid)})"
  [& args]
  `(pco/defmutation ~@args))
