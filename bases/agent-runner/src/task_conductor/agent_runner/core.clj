(ns task-conductor.agent-runner.core
  "Bootstrap namespace for the agent-runner base.
   Requiring this namespace loads all component resolvers and statecharts
   via auto-registration on namespace load."
  (:require
   [clojure.set :as set]
   [clojure.string :as str]
   [task-conductor.claude-cli.resolvers]
   [task-conductor.dev-env.resolvers]
   [task-conductor.emacs-dev-env.interface :as emacs-dev-env]
   [task-conductor.mcp-tasks.resolvers]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.execute]
   [task-conductor.project.resolvers :as resolvers]
   [task-conductor.statechart-engine.interface :as sc]
   [task-conductor.statechart-engine.resolvers]))

(defn- resolver-namespaces
  "Derives resolver namespaces from loaded task-conductor namespaces.
   Avoids maintaining a separate list that duplicates the :require form."
  []
  (into []
        (comp (map ns-name)
              (filter #(let [s (str %)]
                         (and (str/starts-with? s "task-conductor.")
                              (str/ends-with? s ".resolvers")))))
        (all-ns)))

(defn- log-transition
  "Log a state transition at info level."
  [session-id from-state to-state event]
  (println "[agent-runner]"
           {:session-id session-id
            :from from-state
            :to to-state
            :event event}))

(def ^:private session-notify-states
  "States that trigger session notification to dev-envs.
  Includes sub-states so idle↔running transitions within :escalated
  also fire notifications."
  #{:escalated :idle :wait-pr-merge :session-idle :session-running})

(defn- notify-on-session-state-change
  "Push session data to all dev-envs when a session enters or leaves
  a notify-relevant state, including sub-state transitions within
  :escalated (idle↔running)."
  [_session-id from-state to-state _event]
  (let [from-relevant (set/intersection from-state session-notify-states)
        to-relevant   (set/intersection to-state session-notify-states)]
    (when (and (not= from-relevant to-relevant)
               (or (seq from-relevant) (seq to-relevant)))
      (emacs-dev-env/notify-all-sessions-changed!))))

(def ^:private bootstrap-config
  "Configuration stored at bootstrap time.
   Contains :nrepl-port when provided."
  (atom {}))

(defn nrepl-port
  "Return the nREPL port stored at bootstrap time, or nil."
  []
  (:nrepl-port @bootstrap-config))

(defn bootstrap!
  "Log which namespaces were loaded, register transition logging,
   and verify the graph is operational.
   Accepts optional opts map with :nrepl-port to store for session use.
   Returns a map with :namespaces loaded and :graph-operational? status."
  ([] (bootstrap! {}))
  ([opts]
   (reset! bootstrap-config (select-keys opts [:nrepl-port]))
   (sc/add-transition-listener! ::transition-log log-transition)
   (sc/add-transition-listener! ::session-notify notify-on-session-state-change)
   (let [operational? (try
                        (some? (graph/env))
                        (catch Exception _ false))
         ns-list     (resolver-namespaces)]
     (println "[agent-runner] Bootstrap complete:"
              (count ns-list) "resolver namespaces,"
              "graph operational:" operational?)
     {:namespaces ns-list
      :graph-operational? operational?})))

(defn- execute-and-start!
  "Call execute! mutation via EQL, then send initial-state event to
   start the statechart. Returns {:session-id :state :error}."
  [project-dir task-id]
  (let [nrepl-port (nrepl-port)
        result (graph/query
                [`(resolvers/execute!
                   ~(cond-> {:task/project-dir project-dir
                             :task/id task-id}
                      nrepl-port (assoc :task/nrepl-port nrepl-port)))])
        {:execute/keys [session-id initial-state error]}
        (get result `resolvers/execute!)]
    (if error
      (do (println "[agent-runner] execute! failed:"
                   (:error error) (:message error))
          {:session-id nil :state nil :error error})
      (let [state (sc/send! session-id initial-state)]
        (println "[agent-runner] Session" session-id
                 "started, state:" state)
        {:session-id session-id :state state :error nil}))))

(defn run-story!
  "Start supervised execution of a story.
   Kept separate from run-task! to allow divergence when story
   execution adds child-task orchestration.
   Returns {:session-id :state :error}."
  [project-dir story-id]
  (execute-and-start! project-dir story-id))

(defn run-task!
  "Start supervised execution of a standalone task.
   Kept separate from run-story! as standalone tasks skip
   child-task orchestration.
   Returns {:session-id :state :error}."
  [project-dir task-id]
  (execute-and-start! project-dir task-id))

(defn status
  "Returns the current status of a session.
   Returns {:session-id :current-state :history}, or
   {:session-id :error} when the session does not exist."
  [session-id]
  (try
    {:session-id session-id
     :current-state (sc/current-state session-id)
     :history (sc/history session-id)}
    (catch Exception e
      {:session-id session-id
       :error {:error :session-not-found
               :message (ex-message e)}})))
