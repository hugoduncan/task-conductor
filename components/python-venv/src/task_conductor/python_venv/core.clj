(ns task-conductor.python-venv.core
  "Core implementation for Python virtual environment management."
  (:require
   [babashka.process :as p]
   [clojure.java.io :as io]))

(defn get-env
  "Get an environment variable value. Wrapper for System/getenv to enable testing."
  [name]
  (System/getenv name))

(defn resolve-python-cmd
  "Resolve the Python command to use for creating venvs.

   Precedence:
   1. :python-cmd option value if provided
   2. TASK_CONDUCTOR_PYTHON environment variable if set
   3. \"python3\" as the default fallback

   Arguments:
   - opts - options map, may contain :python-cmd key

   Returns the Python command string to use."
  [opts]
  (or (get opts :python-cmd)
      (get-env "TASK_CONDUCTOR_PYTHON")
      "python3"))

(defn python-path
  "Get path to Python executable in venv."
  [venv-path]
  (let [abs-path (.getAbsolutePath (io/file venv-path))]
    (str abs-path "/bin/python")))

(defn pip-path
  "Get path to pip executable in venv."
  [venv-path]
  (let [abs-path (.getAbsolutePath (io/file venv-path))]
    (str abs-path "/bin/pip")))

(defn exists?
  "Check if the Python venv exists."
  [venv-path]
  (.exists (io/file (python-path venv-path))))

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
  [venv-path requirements-path opts]
  (let [quiet? (get opts :quiet? false)
        log (if quiet? (fn [& _]) println)]
    (if (exists? venv-path)
      true
      (try
        (let [abs-venv (.getAbsolutePath (io/file venv-path))
              abs-requirements (.getAbsolutePath (io/file requirements-path))
              python-cmd (resolve-python-cmd opts)]
          (log "Creating Python venv at" abs-venv)
          (let [create-result (p/shell {:out :string :err :string :continue true}
                                       python-cmd "-m" "venv" abs-venv)]
            (if (zero? (:exit create-result))
              (do
                (log "Installing requirements from" abs-requirements)
                (let [pip (pip-path venv-path)
                      install-result (p/shell {:out :string :err :string :continue true}
                                              pip "install" "-r" abs-requirements)]
                  (if (zero? (:exit install-result))
                    (do (log "Venv setup complete")
                        true)
                    (do (log "Failed to install requirements:" (:err install-result))
                        false))))
              (do (log "Failed to create venv:" (:err create-result))
                  false))))
        (catch Exception e
          (log "Venv setup failed:" (.getMessage e))
          false)))))
