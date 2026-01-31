(ns task-conductor.project.registry
  "Registry for managing projects in agent orchestration.
   Projects are keyed by canonical filesystem path."
  (:require
   [clojure.java.io :as io]))

(defonce ^:private registry
  (atom {}))

(defn canonicalize-path
  "Returns canonical absolute path, resolving symlinks."
  [path]
  (.getCanonicalPath (io/file path)))

(defn validate-path
  "Returns error map if path doesn't exist or isn't a directory, nil otherwise."
  [path]
  (let [f (io/file path)]
    (cond
      (not (.exists f))
      {:error :path-not-found
       :message (str "Path does not exist: " path)}

      (not (.isDirectory f))
      {:error :not-a-directory
       :message (str "Path is not a directory: " path)}

      :else nil)))

(defn- path-segment
  "Returns the final segment of a path."
  [path]
  (.getName (io/file path)))

(defn- find-by-name
  "Find project by name in registry map. Returns [path project] or nil."
  [registry-map name]
  (first (filter (fn [[_ project]]
                   (= name (:project/name project)))
                 registry-map)))

(defn get-by-path
  "Lookup project by canonical path. Returns project map or nil."
  [path]
  (get @registry (canonicalize-path path)))

(defn get-by-name
  "Lookup project by name. Returns project map or nil."
  [name]
  (second (find-by-name @registry name)))

(defn list-all
  "Returns seq of all projects."
  []
  (vec (vals @registry)))

(defn register!
  "Creates project at path. Opts may include :project/name.
   Returns project map or error map if duplicate/invalid."
  ([path]
   (register! path {}))
  ([path opts]
   (let [canonical (canonicalize-path path)]
     (if-let [error (validate-path canonical)]
       error
       (let [name (or (:project/name opts) (path-segment canonical))
             result (atom nil)]
         (swap! registry
                (fn [m]
                  (cond
                    (contains? m canonical)
                    (do (reset! result {:error :duplicate-path
                                        :message (str "Project already registered: " canonical)})
                        m)

                    (find-by-name m name)
                    (do (reset! result {:error :duplicate-name
                                        :message (str "Project name already exists: " name)})
                        m)

                    :else
                    (let [project {:project/path canonical
                                   :project/name name}]
                      (reset! result project)
                      (assoc m canonical project)))))
         @result)))))

(defn unregister!
  "Removes project by path. Returns removed project or nil."
  [path]
  (let [canonical (canonicalize-path path)
        [old-state _] (swap-vals! registry dissoc canonical)]
    (get old-state canonical)))

(def ^:private updatable-keys
  "Keys that can be modified via update!"
  #{:project/name})

(defn update!
  "Updates project name. Only :project/name can be modified.
   Returns updated project or error map."
  [path updates]
  (let [canonical (canonicalize-path path)
        allowed-updates (select-keys updates updatable-keys)
        result (atom nil)]
    (swap! registry
           (fn [m]
             (if-let [existing (get m canonical)]
               (let [new-name (:project/name allowed-updates)]
                 (if (and new-name
                          (not= new-name (:project/name existing))
                          (find-by-name m new-name))
                   (do (reset! result {:error :duplicate-name
                                       :message (str "Project name already exists: " new-name)})
                       m)
                   (let [updated (merge existing allowed-updates)]
                     (reset! result updated)
                     (assoc m canonical updated))))
               (do (reset! result {:error :project-not-found
                                   :message (str "Project not found: " canonical)})
                   m))))
    @result))

(defn clear!
  "Resets registry. For testing."
  []
  (reset! registry {})
  nil)
