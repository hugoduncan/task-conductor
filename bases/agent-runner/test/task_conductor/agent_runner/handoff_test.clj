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
    (testing "deletes existing file"
      (with-temp-file
        (fn [path]
          (handoff/write-handoff-state (valid-state) path)
          (is (.exists (File. path)))
          (handoff/clear-handoff-state path)
          (is (not (.exists (File. path)))))))

    (testing "is no-op when file missing"
      (with-temp-file
        (fn [path]
          (is (not (.exists (File. path))))
          (is (nil? (handoff/clear-handoff-state path)))
          (is (not (.exists (File. path)))))))))

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
