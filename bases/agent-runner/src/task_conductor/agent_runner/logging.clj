(ns task-conductor.agent-runner.logging
  "Logging setup for agent-runner.

   Initializes telemere as the logging backend, routing both SLF4J and
   clojure.tools.logging calls through telemere. Require this namespace
   early in application startup to enable log output."
  (:require
   [taoensso.telemere :as t]
   [taoensso.telemere.tools-logging :as tt]))

;; Route tools.logging calls to telemere
(tt/tools-logging->telemere!)

(defn check-interop
  "Return telemere interop status for debugging logging setup."
  []
  (t/check-interop))
