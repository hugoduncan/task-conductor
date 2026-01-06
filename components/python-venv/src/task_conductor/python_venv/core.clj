(ns task-conductor.python-venv.core
  "Core implementation for Python virtual environment management."
  (:require
   [babashka.process :as p]
   [clojure.java.io :as io]))

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
   - :quiet? - suppress output (default: false)

   Returns true if venv is ready, false if creation failed."
  [venv-path requirements-path opts]
  (let [quiet? (get opts :quiet? false)
        log (if quiet? (fn [& _]) println)]
    (if (exists? venv-path)
      true
      (try
        (let [abs-venv (.getAbsolutePath (io/file venv-path))
              abs-requirements (.getAbsolutePath (io/file requirements-path))]
          (log "Creating Python venv at" abs-venv)
          (let [create-result (p/shell {:out :string :err :string :continue true}
                                       "python3" "-m" "venv" abs-venv)]
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
