(ns user
  "Development namespace for task-conductor."
  {:clj-kondo/config '{:linters {:unused-namespace {:level :off}
                                 :unused-referred-var {:level :off}}}}
  (:require [clojure.repl :refer [doc source]]))
