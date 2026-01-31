(ns hooks.dev-env
  (:require [clj-kondo.hooks-api :as api]))

(defn with-session
  "Transform (with-session [name dev-env session-id opts] body)
   into (let [name dev-env session-id opts] body)
   so clj-kondo understands the binding."
  [{:keys [node]}]
  (let [[binding-vec & body] (rest (:children node))
        [name-node dev-env session-id opts] (:children binding-vec)
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   (api/vector-node [name-node
                                     (api/list-node
                                      (list (api/token-node 'do)
                                            dev-env session-id opts))])
                   body))]
    {:node new-node}))
