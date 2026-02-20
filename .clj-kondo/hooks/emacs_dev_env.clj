(ns hooks.emacs-dev-env
  (:require [clj-kondo.hooks-api :as api]))

(defn with-dev-env
  "Transform (with-dev-env binding dev-env-id body...)
   into (let [binding dev-env-id] body...)
   so clj-kondo understands the binding."
  [{:keys [node]}]
  (let [[binding-node dev-env-id-node & body] (rest (:children node))
        new-node (api/list-node
                  (list*
                   (api/token-node 'let)
                   (api/vector-node [binding-node dev-env-id-node])
                   body))]
    {:node new-node}))
