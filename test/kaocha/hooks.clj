(ns kaocha.hooks
  "Kaocha lifecycle hooks for test configuration."
  (:require [taoensso.timbre :as timbre]))

(defn pre-load [config]
  (timbre/set-min-level! :warn)
  config)
