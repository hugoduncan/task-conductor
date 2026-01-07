(ns task-conductor.agent-runner.handoff-test
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.handoff :as handoff])
  (:import
   [java.io File FileNotFoundException]
   [java.time Instant]))

;; Tests for handoff state management functions.
;; Contracts tested:
;; - write-handoff-state validates and atomically persists state
;; - read-handoff-state reads, parses, validates, and returns state
;; - clear-handoff-state removes file or no-ops if missing
;; - Round-trip preserves data including Instant timestamps

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
   :timestamp (Instant/parse "2024-01-15T10:30:00Z")})

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

    (testing "throws on missing required field"
      (with-temp-file
        (fn [path]
          (let [state (dissoc (valid-state) :session-id)]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                  #"Invalid handoff state"
                                  (handoff/write-handoff-state state path)))))))

    (testing "throws on invalid status enum"
      (with-temp-file
        (fn [path]
          (let [state (assoc (valid-state) :status :invalid)]
            (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                  #"Invalid handoff state"
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

    (testing "throws on valid EDN with invalid schema"
      (with-temp-file
        (fn [path]
          (spit path (pr-str {:status :active}))
          (is (thrown-with-msg? clojure.lang.ExceptionInfo
                                #"Invalid handoff state"
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

;; Tests for watch-handoff-file
;; Contracts tested:
;; - Callback invoked on file create/modify with parsed state
;; - Stop function halts the watcher
;; - Read errors during mid-write are silently ignored

(deftest watch-handoff-file-test
  (testing "watch-handoff-file"
    (testing "invokes callback when file is created"
      (with-temp-file
        (fn [path]
          (let [received (atom nil)
                stop-fn (handoff/watch-handoff-file
                         (fn [state] (reset! received state))
                         path)]
            (try
              (Thread/sleep 50) ; Allow watcher to initialize
              (handoff/write-handoff-state (valid-state) path)
              (let [result (wait-for #(deref received) 2000)]
                (is result "callback should be invoked")
                (is (= :active (:status result)))
                (is (= "test-session-123" (:session-id result))))
              (finally
                (stop-fn)))))))

    (testing "invokes callback when file is modified"
      (with-temp-file
        (fn [path]
          (let [states (atom [])
                stop-fn (handoff/watch-handoff-file
                         (fn [state]
                           (swap! states conj state))
                         path)]
            (try
              (Thread/sleep 50)
              (handoff/write-handoff-state (valid-state) path)
              (wait-for #(pos? (count @states)) 2000)
              (handoff/write-handoff-state
               (assoc (valid-state) :status :completed)
               path)
              (wait-for #(= :completed (:status (last @states))) 2000)
              ;; At minimum, at least one callback should fire
              (is (pos? (count @states)) "callback invoked at least once")
              ;; The last observed state should be :completed
              (is (= :completed (:status (last @states)))
                  "final state is :completed")
              (finally
                (stop-fn)))))))

    (testing "returns working stop function"
      (with-temp-file
        (fn [path]
          (let [call-count (atom 0)
                stop-fn (handoff/watch-handoff-file
                         (fn [_] (swap! call-count inc))
                         path)]
            (Thread/sleep 50)
            (stop-fn)
            (Thread/sleep 50)
            (let [count-before @call-count]
              (handoff/write-handoff-state (valid-state) path)
              ;; Short wait to confirm no callback fires
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
              (Thread/sleep 50)
              ;; Write invalid EDN - should not throw or invoke callback
              (spit path "{:invalid}")
              (Thread/sleep 100)
              ;; Now write valid state
              (handoff/write-handoff-state (valid-state) path)
              (let [result (wait-for #(deref received) 2000)]
                (is result "callback invoked with valid state"))
              (finally
                (stop-fn)))))))))

;; Tests for default-handoff-path usage
;; Contracts tested:
;; - 0-arity write-handoff-state uses default-handoff-path
;; - 0-arity read-handoff-state uses default-handoff-path
;; - 1-arity watch-handoff-file uses default-handoff-path

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

          (testing "watch-handoff-file watches default path"
            (let [received (atom nil)
                  stop-fn (handoff/watch-handoff-file
                           (fn [state] (reset! received state)))]
              (try
                (Thread/sleep 50)
                (handoff/write-handoff-state
                 (assoc (valid-state) :status :completed))
                (let [result (wait-for #(deref received) 2000)]
                  (is result "callback invoked")
                  (is (= :completed (:status result))))
                (finally
                  (stop-fn)))))
          (finally
            (.delete file)))))))
