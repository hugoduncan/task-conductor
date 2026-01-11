(ns task-conductor.python-venv.interface
  "Python virtual environment management.

   Provides functions to ensure Python venvs exist and have
   required dependencies installed."
  (:require
   [task-conductor.python-venv.core :as core]))

(defn python-path
  "Get path to Python executable in venv."
  [venv-path]
  (core/python-path venv-path))

(defn pip-path
  "Get path to pip executable in venv."
  [venv-path]
  (core/pip-path venv-path))

(defn exists?
  "Check if the Python venv exists."
  [venv-path]
  (core/exists? venv-path))

(defn ensure!
  "Create the Python venv and install requirements if it doesn't exist.

   Arguments:
   - venv-path - path to venv directory
   - requirements-path - path to requirements.txt

   Options:
   - :python-cmd - Python command or path to use for creating the venv.
                   If not provided, falls back to TASK_CONDUCTOR_PYTHON
                   environment variable, then to \"python3\" as default.
   - :quiet? - suppress output (default: false)

   Returns true if venv is ready, false if creation failed."
  ([venv-path requirements-path]
   (core/ensure! venv-path requirements-path {}))
  ([venv-path requirements-path opts]
   (core/ensure! venv-path requirements-path opts)))
