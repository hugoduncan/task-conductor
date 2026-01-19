(ns task-conductor.workspace.core
  "Core implementation for workspace management."
  (:require
   [babashka.process :as p]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defonce workspace-state
  (atom {:focused-project nil
         :projects #{}}))

(defn set-focused-project!
  [dir]
  (swap! workspace-state assoc :focused-project dir))

(defn focused-project
  []
  (:focused-project @workspace-state))

(defn add-project!
  [dir]
  (swap! workspace-state update :projects conj dir))

(defn remove-project!
  [dir]
  (swap! workspace-state update :projects disj dir))

(defn list-projects
  []
  (:projects @workspace-state))

(defn project-config
  [dir]
  (let [config-file (io/file dir ".task-conductor.edn")]
    (if (.exists config-file)
      (try
        {:ok (edn/read-string (slurp config-file))}
        (catch Exception e
          {:error (str "Failed to parse .task-conductor.edn: " (.getMessage e))}))
      {:ok nil})))

(defn- parse-worktree-block
  "Parse a single worktree block from porcelain output."
  [block]
  (let [lines (str/split-lines block)
        props (into {}
                    (for [line lines
                          :when (not (str/blank? line))]
                      (let [[key & rest] (str/split line #" " 2)]
                        [key (str/join " " rest)])))]
    {:path (get props "worktree")
     :commit (get props "HEAD")
     :branch (when-let [b (get props "branch")]
               (str/replace b #"^refs/heads/" ""))
     :bare? (contains? props "bare")}))

(defn project-worktrees
  [dir]
  (let [config-result (project-config dir)
        main-checkout (if-let [config (:ok config-result)]
                        (or (:main-checkout config) dir)
                        dir)]
    (try
      (let [result (p/sh ["git" "worktree" "list" "--porcelain"]
                         {:dir main-checkout})
            exit-code (:exit result)]
        (if (zero? exit-code)
          (let [blocks (str/split (:out result) #"\n\n")]
            {:ok (mapv parse-worktree-block blocks)})
          {:error (str "git worktree list failed: " (:err result))}))
      (catch Exception e
        {:error (str "Failed to list worktrees: " (.getMessage e))}))))

(defn project-tasks
  [dir]
  (try
    (let [result (p/sh ["mcp-tasks" "list" "--format" "edn"]
                       {:dir dir})
          exit-code (:exit result)]
      (if (zero? exit-code)
        (try
          {:ok (edn/read-string (:out result))}
          (catch Exception e
            {:error (str "Failed to parse mcp-tasks output: " (.getMessage e))}))
        {:error (str "mcp-tasks list failed: " (:err result))}))
    (catch Exception e
      {:error (str "Failed to run mcp-tasks: " (.getMessage e))})))
