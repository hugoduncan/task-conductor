(ns task-conductor.agent-runner.console-integration-test
  ;; Integration tests for console state machine with real process and file I/O.
  ;;
  ;; Contracts tested:
  ;; - p/process with {:inherit true} returns a derefable process
  ;; - Dereferencing yields map with :exit key
  ;; - Exit code reflects actual command result
  ;; - hand-to-cli transitions to :error-recovery on CLI errors
  ;; - hand-to-cli transitions to :error-recovery when hook status is :error
  ;; - hand-to-cli transitions to :error-recovery when hook status is missing
  ;; - Multiple CLI handoff cycles preserve state correctly
  ;; - Edge cases: malformed hook files, empty files
  ;;
  ;; Run with: clj -M:test :integration
  ;; Skip with: SKIP_INTEGRATION_TESTS=1
  (:require
   [babashka.process :as p]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.console :as console]
   [task-conductor.agent-runner.handoff :as handoff])
  (:import
   [java.nio.file Files]
   [java.time Instant]))

;;; Test Utilities

(defn- skip-integration-tests?
  "Check if integration tests should be skipped.
   Set SKIP_INTEGRATION_TESTS=1 to skip explicitly."
  []
  (some? (System/getenv "SKIP_INTEGRATION_TESTS")))

(defmacro with-skip-check
  "Execute body unless SKIP_INTEGRATION_TESTS is set."
  [& body]
  `(if (skip-integration-tests?)
     (testing "skipped (SKIP_INTEGRATION_TESTS set)"
       (is true))
     (do ~@body)))

(defn- with-temp-dir
  "Execute f with a temporary directory, cleaning up afterward."
  [f]
  (let [temp-dir (Files/createTempDirectory
                  "console-integration-test"
                  (into-array java.nio.file.attribute.FileAttribute []))
        temp-path (.toString temp-dir)]
    (try
      (f temp-path)
      (finally
        (doseq [file (reverse (file-seq (io/file temp-path)))]
          (.delete file))))))

(defn- make-cli-simulator-script
  "Generate bash script content that simulates CLI behavior.
   Writes hook status to the specified path and exits with given code."
  [handoff-path exit-code hook-status]
  (let [status-edn (pr-str (handoff/serialize-hook-status hook-status))]
    (str "#!/bin/bash\n"
         "mkdir -p \"$(dirname \"" handoff-path "\")\"\n"
         "echo '" status-edn "' > \"" handoff-path "\"\n"
         "exit " exit-code "\n")))

(defn- make-cli-simulator-no-hook
  "Generate bash script that exits without writing hook status."
  [exit-code]
  (str "#!/bin/bash\n"
       "exit " exit-code "\n"))

(defn- create-cli-simulator!
  "Create an executable CLI simulator script in temp-dir.
   Returns the script path."
  [temp-dir script-content]
  (let [script-path (str temp-dir "/cli-simulator.sh")]
    (spit script-path script-content)
    (shell/sh "chmod" "+x" script-path)
    script-path))

(defn- setup-console-for-handoff!
  "Set up console state for hand-to-cli testing.
   Returns the state after setup."
  [story-id session-id task-id]
  (console/reset-state!)
  (console/transition! :selecting-task {:story-id story-id})
  (console/transition! :running-sdk {:session-id session-id
                                     :current-task-id task-id}))

(defmacro with-cli-simulator
  "Execute body with launch-cli-resume and read-hook-status mocked.
   script-path - path to executable script simulating CLI
   handoff-path - path where script writes hook status"
  [[script-path-sym handoff-path-sym] & body]
  `(let [original-read-hook-status# handoff/read-hook-status]
     (with-redefs [console/launch-cli-resume
                   (fn [_session-id#]
                     (:exit @(p/process ["bash" ~script-path-sym]
                                        {:inherit true})))
                   handoff/read-hook-status
                   (fn [] (original-read-hook-status# ~handoff-path-sym))]
       ~@body)))

;;; Babashka Process API Tests

(deftest babashka-process-api-test
  ;; Tests the babashka.process API pattern used by launch-cli-resume.
  ;; Uses simple shell commands to verify the API contract without
  ;; requiring claude CLI or authentication.
  (with-skip-check
    (testing "babashka.process API"
      (testing "with {:inherit true} option"
        (testing "returns exit code 0 for successful command"
          (let [proc (p/process ["true"] {:inherit true})
                result @proc]
            (is (map? result)
                "dereferenced process should be a map")
            (is (contains? result :exit)
                "result should contain :exit key")
            (is (= 0 (:exit result))
                "true command should exit with 0")))

        (testing "returns non-zero exit code for failing command"
          (let [proc (p/process ["false"] {:inherit true})
                result @proc]
            (is (= 1 (:exit result))
                "false command should exit with 1")))

        (testing "propagates actual exit code"
          (let [proc (p/process ["sh" "-c" "exit 42"] {:inherit true})
                result @proc]
            (is (= 42 (:exit result))
                "should return actual exit code from command")))))))

;;; Error Recovery Path Tests

(deftest error-recovery-cli-error-test
  ;; Tests that hand-to-cli transitions to :error-recovery when CLI exits
  ;; with non-zero code, even if hook status is :completed.
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "error recovery"
          (testing "on non-zero CLI exit code"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  hook-status {:status :completed
                               :timestamp (Instant/now)}
                  script (make-cli-simulator-script handoff-path 1 hook-status)
                  script-path (create-cli-simulator! temp-dir script)]
              (setup-console-for-handoff! 53 "sess-1" 75)
              (with-cli-simulator [script-path handoff-path]
                (let [result (console/hand-to-cli)]
                  (is (= :error-recovery (get-in result [:state :state]))
                      "should transition to :error-recovery")
                  (is (= :cli-error (get-in result [:state :error :type]))
                      "error type should be :cli-error")
                  (is (= 1 (get-in result [:state :error :exit-code]))
                      "should capture exit code"))))))))))

(deftest error-recovery-hook-error-test
  ;; Tests that hand-to-cli transitions to :error-recovery when hook
  ;; status is :error, even if CLI exits with 0.
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "error recovery"
          (testing "on hook status :error"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  hook-status {:status :error
                               :timestamp (Instant/now)
                               :reason :tool-error}
                  script (make-cli-simulator-script handoff-path 0 hook-status)
                  script-path (create-cli-simulator! temp-dir script)]
              (setup-console-for-handoff! 53 "sess-2" 76)
              (with-cli-simulator [script-path handoff-path]
                (let [result (console/hand-to-cli)]
                  (is (= :error-recovery (get-in result [:state :state]))
                      "should transition to :error-recovery")
                  (is (= :hook-error (get-in result [:state :error :type]))
                      "error type should be :hook-error")
                  (is (= :error (get-in result [:state :error :hook-status :status]))
                      "should include hook status in error"))))))))))

(deftest error-recovery-cli-killed-test
  ;; Tests that hand-to-cli transitions to :error-recovery with :cli-killed
  ;; when hook status file is missing (user killed CLI or hook didn't run).
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "error recovery"
          (testing "when hook status is missing (CLI killed)"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  script (make-cli-simulator-no-hook 0)
                  script-path (create-cli-simulator! temp-dir script)]
              (setup-console-for-handoff! 53 "sess-3" 77)
              (with-cli-simulator [script-path handoff-path]
                (let [result (console/hand-to-cli)]
                  (is (= :error-recovery (get-in result [:state :state]))
                      "should transition to :error-recovery")
                  (is (= :cli-killed (get-in result [:state :error :type]))
                      "error type should be :cli-killed")
                  (is (nil? (:cli-status result))
                      ":cli-status should be nil"))))))))))

(deftest successful-handoff-test
  ;; Tests that hand-to-cli returns to :running-sdk on successful CLI exit
  ;; with :completed hook status.
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "successful handoff"
          (testing "returns to :running-sdk with :completed hook status"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  hook-status {:status :completed
                               :timestamp (Instant/now)}
                  script (make-cli-simulator-script handoff-path 0 hook-status)
                  script-path (create-cli-simulator! temp-dir script)]
              (setup-console-for-handoff! 53 "sess-4" 78)
              (with-cli-simulator [script-path handoff-path]
                (let [result (console/hand-to-cli)]
                  (is (= :running-sdk (get-in result [:state :state]))
                      "should return to :running-sdk")
                  (is (= :completed (get-in result [:cli-status :status]))
                      "should return :completed cli-status"))))))))))

;;; Multiple Handoff Cycle Tests

(deftest multiple-handoff-cycles-test
  ;; Tests that multiple sequential CLI handoffs work correctly,
  ;; preserving state across cycles.
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "multiple handoff cycles"
          (testing "preserve state across sequential handoffs"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  make-script (fn [exit-code]
                                (let [hook-status {:status :completed
                                                   :timestamp (Instant/now)}]
                                  (make-cli-simulator-script
                                   handoff-path exit-code hook-status)))]
              (setup-console-for-handoff! 53 "sess-multi" 79)

              ;; First handoff - successful
              (let [script1 (create-cli-simulator! temp-dir (make-script 0))]
                (with-cli-simulator [script1 handoff-path]
                  (let [result1 (console/hand-to-cli)]
                    (is (= :running-sdk (get-in result1 [:state :state]))
                        "first handoff should return to :running-sdk")
                    (is (= "sess-multi" (get-in result1 [:state :session-id]))
                        "should preserve session-id")
                    (is (= 53 (get-in result1 [:state :story-id]))
                        "should preserve story-id"))))

              ;; Second handoff - successful
              (let [script2 (create-cli-simulator! temp-dir (make-script 0))]
                (with-cli-simulator [script2 handoff-path]
                  (let [result2 (console/hand-to-cli)]
                    (is (= :running-sdk (get-in result2 [:state :state]))
                        "second handoff should return to :running-sdk")
                    (is (= "sess-multi" (get-in result2 [:state :session-id]))
                        "should still preserve session-id"))))

              ;; Third handoff - error, should go to error-recovery
              (let [script3 (create-cli-simulator! temp-dir (make-script 1))]
                (with-cli-simulator [script3 handoff-path]
                  (let [result3 (console/hand-to-cli)]
                    (is (= :error-recovery (get-in result3 [:state :state]))
                        "third handoff should error")
                    (is (= "sess-multi" (get-in result3 [:state :session-id]))
                        "should preserve session-id even in error")))))))))))

(deftest handoff-history-tracking-test
  ;; Tests that state machine history correctly tracks all transitions
  ;; through a handoff cycle.
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "handoff history tracking"
          (testing "records all transitions through handoff cycle"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  hook-status {:status :completed :timestamp (Instant/now)}
                  script (make-cli-simulator-script handoff-path 0 hook-status)
                  script-path (create-cli-simulator! temp-dir script)]
              (setup-console-for-handoff! 53 "sess-history" 80)
              (let [pre-history-count (count (console/state-history))]
                (with-cli-simulator [script-path handoff-path]
                  (console/hand-to-cli)
                  (let [history (console/state-history)
                        handoff-entries (drop pre-history-count history)]
                    (is (= 3 (count handoff-entries))
                        "handoff should add 3 transitions")
                    (is (= [:running-sdk :needs-input :running-cli]
                           (mapv :from handoff-entries))
                        "should transition through expected states")
                    (is (= [:needs-input :running-cli :running-sdk]
                           (mapv :to handoff-entries))
                        "should end at :running-sdk")))))))))))

;;; Edge Case Tests

(deftest malformed-hook-status-test
  ;; Tests that malformed hook status file is treated as missing,
  ;; triggering :cli-killed error recovery.
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "edge cases"
          (testing "malformed hook status file treated as missing"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  ;; Script writes invalid EDN
                  script (str "#!/bin/bash\n"
                              "mkdir -p \"$(dirname \"" handoff-path "\")\"\n"
                              "echo '{:invalid edn missing bracket' > \""
                              handoff-path "\"\n"
                              "exit 0\n")
                  script-path (create-cli-simulator! temp-dir script)]
              (setup-console-for-handoff! 53 "sess-malformed" 81)
              (with-cli-simulator [script-path handoff-path]
                (let [result (console/hand-to-cli)]
                  (is (= :error-recovery (get-in result [:state :state]))
                      "should transition to :error-recovery")
                  (is (= :cli-killed (get-in result [:state :error :type]))
                      "malformed file should be treated as missing"))))))))))

(deftest invalid-schema-hook-status-test
  ;; Tests that hook status with invalid schema is treated as missing.
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "edge cases"
          (testing "invalid schema hook status treated as missing"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  ;; Script writes valid EDN but invalid schema (missing timestamp)
                  script (str "#!/bin/bash\n"
                              "mkdir -p \"$(dirname \"" handoff-path "\")\"\n"
                              "echo '{:status :completed}' > \"" handoff-path "\"\n"
                              "exit 0\n")
                  script-path (create-cli-simulator! temp-dir script)]
              (setup-console-for-handoff! 53 "sess-invalid-schema" 82)
              (with-cli-simulator [script-path handoff-path]
                (let [result (console/hand-to-cli)]
                  (is (= :error-recovery (get-in result [:state :state]))
                      "should transition to :error-recovery")
                  (is (= :cli-killed (get-in result [:state :error :type]))
                      "invalid schema should be treated as missing"))))))))))

(deftest empty-hook-status-file-test
  ;; Tests that empty hook status file is treated as missing.
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "edge cases"
          (testing "empty hook status file treated as missing"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  script (str "#!/bin/bash\n"
                              "mkdir -p \"$(dirname \"" handoff-path "\")\"\n"
                              "touch \"" handoff-path "\"\n"
                              "exit 0\n")
                  script-path (create-cli-simulator! temp-dir script)]
              (setup-console-for-handoff! 53 "sess-empty" 83)
              (with-cli-simulator [script-path handoff-path]
                (let [result (console/hand-to-cli)]
                  (is (= :error-recovery (get-in result [:state :state]))
                      "should transition to :error-recovery")
                  (is (= :cli-killed (get-in result [:state :error :type]))
                      "empty file should be treated as missing"))))))))))

(deftest extended-hook-status-test
  ;; Tests that extended hook status fields are preserved through handoff.
  (with-skip-check
    (with-temp-dir
      (fn [temp-dir]
        (testing "edge cases"
          (testing "extended hook status fields preserved"
            (let [handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                  hook-status {:status :needs-clarification
                               :timestamp (Instant/now)
                               :reason :ambiguous-task
                               :question "Which file?"}
                  script (make-cli-simulator-script handoff-path 0 hook-status)
                  script-path (create-cli-simulator! temp-dir script)]
              (setup-console-for-handoff! 53 "sess-extended" 84)
              (with-cli-simulator [script-path handoff-path]
                (let [result (console/hand-to-cli)]
                  (is (= :running-sdk (get-in result [:state :state]))
                      "non-error status should return to :running-sdk")
                  (is (= :needs-clarification (get-in result [:cli-status :status]))
                      "should preserve :status")
                  (is (= :ambiguous-task (get-in result [:cli-status :reason]))
                      "should preserve :reason")
                  (is (= "Which file?" (get-in result [:cli-status :question]))
                      "should preserve :question"))))))))))
