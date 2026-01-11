(ns task-conductor.agent-runner.handoff-test
  ;; Tests for handoff state management functions and HookStatus schemas.
  ;;
  ;; Contracts tested:
  ;; - write-handoff-state validates and atomically persists state
  ;; - read-handoff-state reads, parses, validates, and returns state
  ;; - clear-handoff-state removes file or no-ops if missing
  ;; - Round-trip preserves data including Instant timestamps
  ;; - HookStatus schema validates required and optional fields
  ;; - HookStatus serialization round-trips preserve data
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.handoff :as handoff])
  (:import
   [java.io File FileNotFoundException]
   [java.time Instant]))

;;; Test Utilities

(defn- skip-file-watcher-tests?
  "Check if file watcher tests should be skipped.
   File watchers may not work reliably in CI environments.
   Set SKIP_FILE_WATCHER_TESTS=1 to skip."
  []
  (or (some? (System/getenv "CI"))
      (some? (System/getenv "SKIP_FILE_WATCHER_TESTS"))))

(defmacro with-file-watcher-skip
  "Execute body unless file watcher tests should be skipped."
  [& body]
  `(if (skip-file-watcher-tests?)
     (testing "skipped (file watcher tests disabled in CI)"
       (is true))
     (do ~@body)))

(defn- with-temp-file
  "Execute f with a temp file path, ensuring cleanup."
  [f]
  (let [file (File/createTempFile "handoff-test" ".edn")]
    (.delete file)
    (try
      (f (.getAbsolutePath file))
      (finally
        (.delete file)))))

(defn- wait-for
  "Poll condition-fn at interval-ms until truthy or timeout-ms exceeded.
  Returns the truthy result or nil on timeout."
  ([condition-fn]
   (wait-for condition-fn 1000 20))
  ([condition-fn timeout-ms]
   (wait-for condition-fn timeout-ms 20))
  ([condition-fn timeout-ms interval-ms]
   (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
     (loop []
       (if-let [result (condition-fn)]
         result
         (if (> (System/currentTimeMillis) deadline)
           nil
           (do
             (Thread/sleep interval-ms)
             (recur))))))))

(defn- valid-state
  "Create a valid handoff state for testing."
  []
  {:status :active
   :session-id "test-session-123"
   :task-id 42
   :story-id 10
   :timestamp (Instant/now)})

(defn- poll-until-ready!
  "Poll-write until ready-pred returns truthy. Used to ensure watcher is ready.
  Returns true if ready-pred returned truthy, false on timeout."
  [ready-pred path timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      ;; Create fresh state each write to ensure unique content (different timestamp)
      (handoff/write-handoff-state (valid-state) path)
      (if (wait-for ready-pred 200)
        true
        (if (< (System/currentTimeMillis) deadline)
          (recur)
          false)))))

(defn- wait-for-watcher-ready!
  "Wait for watcher to be ready by poll-writing until callback fires.
  Returns true if watcher responded, false on timeout."
  [received-atom path timeout-ms]
  (poll-until-ready! #(some? @received-atom) path timeout-ms))

;;; HandoffState Tests

(deftest write-handoff-state-test
  (testing "write-handoff-state"
    (testing "writes valid state to file"
      (with-temp-file
        (fn [path]
          (let [state (valid-state)
                result (handoff/write-handoff-state state path)]
            (is (= state result) "returns the input state")
            (is (.exists (File. path)) "creates file")))))

    (testing "throws on invalid state"
      (with-temp-file
        (fn [path]
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"Invalid handoff state"
                                (handoff/write-handoff-state {} path))))))

    (testing "throws on missing required field with field name in message"
      (with-temp-file
        (fn [path]
          (let [state (dissoc (valid-state) :session-id)]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                  #"Invalid handoff state:.*:session-id"
                                  (handoff/write-handoff-state state path)))))))

    (testing "throws on invalid status enum with field name in message"
      (with-temp-file
        (fn [path]
          (let [state (assoc (valid-state) :status :invalid)]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                  #"Invalid handoff state:.*:status"
                                  (handoff/write-handoff-state state path)))))))

    (testing "accepts optional fields"
      (with-temp-file
        (fn [path]
          (let [state (assoc (valid-state)
                             :handoff-reason "needs user input"
                             :sdk-result {:foo "bar"})]
            (is (= state (handoff/write-handoff-state state path)))))))))

(deftest read-handoff-state-test
  (testing "read-handoff-state"
    (testing "reads valid state from file"
      (with-temp-file
        (fn [path]
          (let [state (valid-state)]
            (handoff/write-handoff-state state path)
            (is (= state (handoff/read-handoff-state path)))))))

    (testing "preserves optional fields through round-trip"
      (with-temp-file
        (fn [path]
          (let [state (assoc (valid-state)
                             :handoff-reason "test reason"
                             :sdk-result {:key "value"})]
            (handoff/write-handoff-state state path)
            (is (= state (handoff/read-handoff-state path)))))))

    (testing "throws FileNotFoundException when file missing"
      (is (thrown? FileNotFoundException
                   (handoff/read-handoff-state "/nonexistent/path.edn"))))

    (testing "throws on corrupt EDN"
      (with-temp-file
        (fn [path]
          (spit path "{:invalid edn missing bracket")
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"Failed to parse"
                                (handoff/read-handoff-state path))))))

    (testing "throws on valid EDN with invalid schema with field name in message"
      (with-temp-file
        (fn [path]
          (spit path (pr-str {:status :active}))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"Invalid handoff state:.*:session-id"
                                (handoff/read-handoff-state path))))))))

(deftest clear-handoff-state-test
  (testing "clear-handoff-state"
    (testing "when file exists"
      (testing "deletes it and returns true"
        (with-temp-file
          (fn [path]
            (handoff/write-handoff-state (valid-state) path)
            (is (.exists (File. path)))
            (is (true? (handoff/clear-handoff-state path)))
            (is (not (.exists (File. path))))))))

    (testing "when file missing"
      (testing "returns false"
        (with-temp-file
          (fn [path]
            (is (not (.exists (File. path))))
            (is (false? (handoff/clear-handoff-state path)))
            (is (not (.exists (File. path))))))))))

(deftest round-trip-test
  (testing "round-trip"
    (testing "preserves Instant timestamp precisely"
      (with-temp-file
        (fn [path]
          (let [ts (Instant/now)
                state (assoc (valid-state) :timestamp ts)]
            (handoff/write-handoff-state state path)
            (is (= ts (:timestamp (handoff/read-handoff-state path))))))))

    (testing "preserves all status enum values"
      (with-temp-file
        (fn [path]
          (doseq [status [:active :needs-input :completed :error]]
            (let [state (assoc (valid-state) :status status)]
              (handoff/write-handoff-state state path)
              (is (= status (:status (handoff/read-handoff-state path)))))))))))

;;; File Watching Tests

(deftest watch-handoff-file-test
  (with-file-watcher-skip
    (testing "watch-handoff-file"
      (testing "invokes callback when file is created"
        (with-temp-file
          (fn [path]
            (let [received (atom nil)
                  stop-fn (handoff/watch-handoff-file
                           (fn [state] (reset! received state))
                           path)]
              (try
                (is (wait-for-watcher-ready! received path 2000)
                    "watcher should initialize and invoke callback")
                (is (= :active (:status @received)))
                (is (= "test-session-123" (:session-id @received)))
                (finally
                  (stop-fn))))))))

    (testing "invokes callback when file is modified"
      (with-temp-file
        (fn [path]
          (let [states (atom [])
                stop-fn (handoff/watch-handoff-file
                         (fn [state]
                           (swap! states conj state))
                         path)]
            (try
              ;; Ensure watcher is ready
              (is (poll-until-ready! #(pos? (count @states)) path 2000)
                  "watcher should initialize")
              ;; Write modified state
              (handoff/write-handoff-state
               (assoc (valid-state) :status :completed)
               path)
              (is (wait-for #(= :completed (:status (last @states))) 2000)
                  "should receive :completed state")
              (finally
                (stop-fn)))))))

    (testing "returns working stop function"
      (with-temp-file
        (fn [path]
          (let [call-count (atom 0)
                stop-fn (handoff/watch-handoff-file
                         (fn [_] (swap! call-count inc))
                         path)]
            ;; First confirm watcher is working
            (is (poll-until-ready! #(pos? @call-count) path 2000)
                "watcher should initialize and fire")
            (let [count-before @call-count]
              (stop-fn)
              ;; Write after stop
              (handoff/write-handoff-state
               (assoc (valid-state) :status :completed)
               path)
              ;; Brief wait then verify count unchanged
              (Thread/sleep 200)
              (is (= count-before @call-count)
                  "callback not invoked after stop"))))))

    (testing "ignores read errors silently"
      (with-temp-file
        (fn [path]
          (let [received (atom nil)
                stop-fn (handoff/watch-handoff-file
                         (fn [state] (reset! received state))
                         path)]
            (try
              ;; Ensure watcher is ready
              (is (wait-for-watcher-ready! received path 2000)
                  "watcher should initialize")
              (reset! received nil)
              ;; Write invalid EDN - should not throw or invoke callback
              (spit path "{:invalid}")
              ;; Write valid state with different status to distinguish
              (handoff/write-handoff-state
               (assoc (valid-state) :status :completed)
               path)
              (is (wait-for #(= :completed (:status @received)) 2000)
                  "callback invoked with valid state after invalid EDN")
              (finally
                (stop-fn)))))))))

;;; Default Path Tests

(deftest default-path-test
  (testing "default-handoff-path usage"
    (testing "when no path provided"
      (let [default-path handoff/default-handoff-path
            file (File. default-path)]
        (try
          (testing "write-handoff-state writes to default path"
            (handoff/write-handoff-state (valid-state))
            (is (.exists file) "file created at default path"))

          (testing "read-handoff-state reads from default path"
            (let [state (handoff/read-handoff-state)]
              (is (= :active (:status state)))
              (is (= "test-session-123" (:session-id state)))))

          (if (skip-file-watcher-tests?)
            (testing "watch-handoff-file watches default path (skipped in CI)"
              (is true))
            (testing "watch-handoff-file watches default path"
              (let [received (atom nil)
                    stop-fn (handoff/watch-handoff-file
                             (fn [state] (reset! received state)))]
                (try
                  ;; Ensure watcher is ready by poll-writing
                  (is (poll-until-ready! #(some? @received)
                                         handoff/default-handoff-path
                                         2000)
                      "watcher should initialize")
                  (reset! received nil)
                  (handoff/write-handoff-state
                   (assoc (valid-state) :status :completed))
                  (is (wait-for #(= :completed (:status @received)) 2000)
                      "callback invoked with :completed status")
                  (finally
                    (stop-fn))))))
          (finally
            (.delete file)))))))

;;; HookStatus Schema Tests

(def valid-hook-status
  {:status :completed
   :timestamp (Instant/parse "2024-01-15T10:30:00Z")})

(def valid-hook-status-extended
  {:status :needs-clarification
   :timestamp (Instant/parse "2024-01-15T10:30:00Z")
   :reason :ambiguous-task
   :question "Which file?"})

(deftest hook-status-schema-test
  (testing "HookStatus"
    (testing "validates minimal valid status"
      (is (handoff/valid-hook-status? valid-hook-status)))

    (testing "validates status with optional fields"
      (is (handoff/valid-hook-status? valid-hook-status-extended)))

    (testing "allows custom extension fields"
      (is (handoff/valid-hook-status?
           (assoc valid-hook-status :custom-field "value"))))

    (testing "rejects missing :status"
      (is (not (handoff/valid-hook-status?
                {:timestamp (Instant/now)}))))

    (testing "rejects missing :timestamp"
      (is (not (handoff/valid-hook-status?
                {:status :completed}))))

    (testing "rejects wrong type for :status"
      (is (not (handoff/valid-hook-status?
                {:status "completed"
                 :timestamp (Instant/now)}))))

    (testing "rejects wrong type for :timestamp"
      (is (not (handoff/valid-hook-status?
                {:status :completed
                 :timestamp "not-an-instant"}))))))

(deftest hook-status-explanation-test
  (testing "explain-hook-status"
    (testing "returns nil for valid status"
      (is (nil? (handoff/explain-hook-status valid-hook-status))))

    (testing "returns errors for missing required fields"
      (let [explanation (handoff/explain-hook-status {})]
        (is (some? explanation))
        (is (contains? explanation :status))
        (is (contains? explanation :timestamp))))))

;;; HookStatus Serialization Tests

(deftest hook-status-serialization-test
  (testing "HookStatus serialization"
    (testing "round-trips minimal status"
      (let [serialized (handoff/serialize-hook-status valid-hook-status)
            deserialized (handoff/deserialize-hook-status serialized)]
        (is (= valid-hook-status deserialized))))

    (testing "round-trips extended status"
      (let [serialized (handoff/serialize-hook-status valid-hook-status-extended)
            deserialized (handoff/deserialize-hook-status serialized)]
        (is (= valid-hook-status-extended deserialized))))

    (testing "serialized format has string timestamp"
      (let [serialized (handoff/serialize-hook-status valid-hook-status)]
        (is (string? (:timestamp serialized)))
        (is (= "2024-01-15T10:30:00Z" (:timestamp serialized)))))))

;;; HookStatus Constructor Tests

(deftest make-hook-status-test
  (testing "make-hook-status"
    (testing "creates valid status with minimal args"
      (let [status (handoff/make-hook-status :completed)]
        (is (handoff/valid-hook-status? status))
        (is (= :completed (:status status)))
        (is (inst? (:timestamp status)))))

    (testing "creates valid status with options"
      (let [status (handoff/make-hook-status :error {:reason :tool-error})]
        (is (handoff/valid-hook-status? status))
        (is (= :error (:status status)))
        (is (= :tool-error (:reason status)))))))

;;; HookStatus File I/O Tests

(deftest read-hook-status-test
  (testing "read-hook-status"
    (testing "returns nil for non-existent file"
      (is (nil? (handoff/read-hook-status "/nonexistent/path.edn"))))

    (testing "reads valid hook status from file"
      (with-temp-file
        (fn [path]
          (spit path (pr-str (handoff/serialize-hook-status valid-hook-status)))
          (let [read-status (handoff/read-hook-status path)]
            (is (= valid-hook-status read-status))))))

    (testing "reads extended hook status from file"
      (with-temp-file
        (fn [path]
          (spit path (pr-str (handoff/serialize-hook-status valid-hook-status-extended)))
          (let [read-status (handoff/read-hook-status path)]
            (is (= valid-hook-status-extended read-status))))))

    (testing "returns nil for invalid EDN"
      (with-temp-file
        (fn [path]
          (spit path "{:invalid edn")
          (is (nil? (handoff/read-hook-status path))))))

    (testing "returns nil for invalid hook status schema"
      (with-temp-file
        (fn [path]
          (spit path (pr-str {:status "invalid"}))
          (is (nil? (handoff/read-hook-status path))))))))
