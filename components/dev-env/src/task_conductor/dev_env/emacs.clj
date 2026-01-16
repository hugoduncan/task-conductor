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

(defn- notify-all-callbacks-error!
  "Notify all pending callbacks of an error condition.
   Clears the callbacks atom after notification."
  [callbacks message]
  (doseq [[session-id callback] @callbacks]
    (try
      (callback {:session-id session-id
                 :status :error
                 :message message})
      (catch Exception _)))
  (reset! callbacks {}))

(defn- start-reader-thread
  "Start background thread that reads messages and dispatches to callbacks.
   Detects connection loss and notifies all pending callbacks.
   Handles thread interruption gracefully during shutdown.
   Returns the Thread object."
  [channel-atom callbacks running]
  (let [thread (Thread.
                (fn []
                  ;; Create a single reader instance to maintain buffer across calls
                  (let [read-message! (socket/make-message-reader @channel-atom)]
                    (loop []
                      (when (and @running (not (Thread/interrupted)))
                        (let [msg (read-message!)]
                          (cond
                            ;; Thread was interrupted or running flag cleared - clean exit
                            (or (Thread/interrupted) (not @running))
                            nil

                            ;; Connection closed or error - notify callbacks
                            (nil? msg)
                            (when @running
                              (binding [*out* *err*]
                                (println "Connection to Emacs lost"))
                              (notify-all-callbacks-error! callbacks "Connection lost")
                              (reset! running false))

                            ;; Parse error - log and continue
                            (:error msg)
                            (do
                              (binding [*out* *err*]
                                (println "Socket parse error:" (:raw msg)))
                              (recur))

                            ;; Valid message - dispatch and continue
                            :else
                            (do
                              (dispatch-message callbacks msg)
                              (recur))))))))
                "emacs-dev-env-reader")]
    (.setDaemon thread true)
    (.start thread)
    thread))

(defn- try-send-with-reconnect!
  "Try to send message, reconnecting once on failure.
   Returns true if sent successfully, false otherwise."
  [channel-atom socket-path message]
  (or (socket/send-message! @channel-atom message)
      ;; First attempt failed, try to reconnect
      (when-let [new-channel (socket/connect! socket-path)]
        (socket/disconnect! @channel-atom)
        (reset! channel-atom new-channel)
        (socket/send-message! new-channel message))))

(defrecord EmacsDevEnv [channel-atom socket-path callbacks running reader-thread]
  dev-env/DevEnv

  (open-cli-session [_this opts callback]
    (let [session-id (:session-id opts)]
      (if-not session-id
        ;; Validation: session-id is required for callback lookup
        (do
          (callback {:session-id nil
                     :status :error
                     :message "Missing required :session-id in opts"})
          {:status :error})
        (do
          (swap! callbacks assoc session-id callback)
          (let [sent? (try-send-with-reconnect!
                       channel-atom
                       socket-path
                       {:type "open-session"
                        :session-id session-id
                        :prompt (:prompt opts)
                        :working-dir (:working-dir opts)})]
            (when-not sent?
              (swap! callbacks dissoc session-id)
              (callback {:session-id session-id
                         :status :error
                         :message "Failed to send message to Emacs"})))
          {:status :requested}))))

  (close-session [_this session-id]
    (try-send-with-reconnect!
     channel-atom
     socket-path
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
     (let [channel-atom (atom channel)
           callbacks (atom {})
           running (atom true)
           reader-thread (start-reader-thread channel-atom callbacks running)]
       (->EmacsDevEnv channel-atom path callbacks running reader-thread)))))

(def ^:private join-timeout-ms
  "Timeout in milliseconds to wait for reader thread termination."
  1000)

(defn stop!
  "Stop the EmacsDevEnv, closing the socket and stopping the reader thread.
   Any pending callbacks will receive an error result.
   Waits up to 1 second for the reader thread to terminate."
  [^EmacsDevEnv env]
  (reset! (:running env) false)
  ;; Interrupt the reader thread to unblock any pending read
  (.interrupt ^Thread (:reader-thread env))
  (socket/disconnect! @(:channel-atom env))
  ;; Wait for reader thread to terminate (with timeout)
  (try
    (.join ^Thread (:reader-thread env) join-timeout-ms)
    (catch InterruptedException _))
  ;; Notify pending callbacks of shutdown
  (doseq [[session-id callback] @(:callbacks env)]
    (try
      (callback {:session-id session-id
                 :status :error
                 :message "DevEnv shutdown"})
      (catch Exception _)))
  (reset! (:callbacks env) {})
  nil)
