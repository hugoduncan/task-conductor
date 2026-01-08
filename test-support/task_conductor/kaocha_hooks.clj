(ns task-conductor.kaocha-hooks
  "Kaocha hooks for test setup."
  (:require
   [task-conductor.python-venv.interface :as venv]))

(def ^:private venv-path
  "Relative path to venv from project root."
  "components/claude-agent-sdk/.venv")

(def ^:private requirements-path
  "Relative path to requirements.txt from project root."
  "components/claude-agent-sdk/requirements.txt")

(defn pre-run
  "Kaocha pre-run hook to ensure Python venv exists.
   Called before any tests run."
  [test-plan]
  (println "Setting up Python venv for tests...")
  (if (venv/ensure! venv-path requirements-path {:quiet? true})
    (println "Python venv ready:" venv-path)
    (println "Warning: Failed to set up Python venv"))
  test-plan)
