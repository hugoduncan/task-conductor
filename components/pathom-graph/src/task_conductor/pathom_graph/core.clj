(ns task-conductor.pathom-graph.core
  "Pathom3 environment with dynamic resolver registry.

   Resolvers and mutations register here. The statechart engine uses
   this for EQL-based action execution."
  (:require
   [com.wsscode.pathom3.connect.indexes :as pci]
   [com.wsscode.pathom3.connect.operation :as pco]
   [com.wsscode.pathom3.interface.eql :as p.eql]))

;;; Registry

(defonce ^:private registry (atom {}))
(defonce ^:private env-cache (atom nil))

(defn- op-name [op]
  (::pco/op-name (pco/operation-config op)))

(defn register!
  "Register resolver(s) or mutation(s). Accepts single op or collection.
   Replaces existing ops with same name. Invalidates env cache."
  [ops]
  (let [ops (if (sequential? ops) ops [ops])
        new-ops (into {} (map (juxt op-name identity)) ops)]
    (swap! registry merge new-ops)
    (reset! env-cache nil))
  :registered)

(defn env
  "Returns current Pathom environment from registered operations.
   Caches result until registry changes."
  []
  (or @env-cache
      (let [ops (vals @registry)
            e (if (seq ops)
                (pci/register ops)
                {})]
        (clojure.core/reset! env-cache e)
        e)))

;;; Query

(defn query
  "Execute EQL query against the current environment.
   Returns result map. Optionally accepts entity data for parameterized queries."
  ([eql]
   (p.eql/process (env) eql))
  ([entity eql]
   (p.eql/process (env) entity eql)))

;;; Testing

(defn reset-graph!
  "Clear all registered operations. For testing only."
  []
  (clojure.core/reset! registry {})
  (clojure.core/reset! env-cache nil)
  :reset)
