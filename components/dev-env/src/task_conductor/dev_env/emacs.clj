(ns task-conductor.dev-env.emacs
  "Emacs implementation of DevEnv protocol.

   Communicates with Emacs via UNIX domain socket to open/close
   interactive Claude CLI sessions. Manages pending callbacks and
   dispatches responses from a background reader thread."
  (:require
   [task-conductor.dev-env.interface :as dev-env]
   [task-conductor.dev-env.socket :as socket]))

(defn- dispatch-message
  "Dispatch an incoming message to the appropriate callback.
   Removes callback from pending map after invocation."
  [callbacks msg]
  (let [session-id (:session-id msg)
        callback (get @callbacks session-id)]
    (when callback
      (swap! callbacks dissoc session-id)
      (try
        (case (:type msg)
          "session-complete"
          (callback {:session-id session-id
                     :status (keyword (:status msg))
                     :hook-status (:hook-status msg)
                     :exit-code (:exit-code msg)})

          "error"
          (callback {:session-id session-id
                     :status :error
                     :message (:message msg)})

          ;; Unknown message type
          (callback {:session-id session-id
                     :status :error
                     :message (str "Unknown message type: " (:type msg))}))
        (catch Exception e
          ;; Log but don't propagate callback exceptions
          (binding [*out* *err*]
            (println "Callback exception for session" session-id ":" (.getMessage e))))))))

(defn- start-reader-thread
  "Start background thread that reads messages and dispatches to callbacks.
   Returns the Thread object."
  [channel callbacks running]
  (let [thread (Thread.
                (fn []
                  (while @running
                    (when-let [msg (socket/receive-message! channel)]
                      (if (:error msg)
                        (binding [*out* *err*]
                          (println "Socket parse error:" (:raw msg)))
                        (dispatch-message callbacks msg)))))
                "emacs-dev-env-reader")]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defrecord EmacsDevEnv [channel callbacks running reader-thread]
  dev-env/DevEnv

  (open-cli-session [_this opts callback]
    (let [session-id (:session-id opts)]
      (swap! callbacks assoc session-id callback)
      (let [sent? (socket/send-message!
                   channel
                   {:type "open-session"
                    :session-id session-id
                    :prompt (:prompt opts)
                    :working-dir (:working-dir opts)})]
        (when-not sent?
          (swap! callbacks dissoc session-id)
          (callback {:session-id session-id
                     :status :error
                     :message "Failed to send message to Emacs"})))
      {:status :requested}))

  (close-session [_this session-id]
    (socket/send-message!
     channel
     {:type "close-session"
      :session-id session-id})
    {:status :requested}))

(defn create-emacs-dev-env
  "Create an EmacsDevEnv connected to the socket at path.
   Starts a background reader thread for incoming messages.

   path - socket file path (defaults to socket/default-socket-path)

   Returns the EmacsDevEnv on success, nil if connection fails."
  ([]
   (create-emacs-dev-env socket/default-socket-path))
  ([path]
   (when-let [channel (socket/connect! path)]
     (let [callbacks (atom {})
           running (atom true)
           reader-thread (start-reader-thread channel callbacks running)]
       (->EmacsDevEnv channel callbacks running reader-thread)))))

(defn stop!
  "Stop the EmacsDevEnv, closing the socket and stopping the reader thread.
   Any pending callbacks will receive an error result."
  [^EmacsDevEnv env]
  (reset! (:running env) false)
  (socket/disconnect! (:channel env))
  ;; Notify pending callbacks of shutdown
  (doseq [[session-id callback] @(:callbacks env)]
    (try
      (callback {:session-id session-id
                 :status :error
                 :message "DevEnv shutdown"})
      (catch Exception _)))
  (reset! (:callbacks env) {})
  nil)
