(ns task-conductor.agent-runner.hooks-test
  ;; Tests for stop and idle hook generation and installation.
  ;;
  ;; Contracts tested:
  ;; - generate-stop-hook produces valid bash script
  ;; - generate-idle-hook produces valid bash script
  ;; - Generated scripts write valid HookStatus EDN
  ;; - install-stop-hook creates executable file
  ;; - install-idle-hook creates executable file
  ;; - configure-claude-settings creates valid JSON with Stop and Notification hooks
  ;; - ensure-hooks-installed is idempotent
  ;; - Full hook flow: install → execute → read status
  (:require
   [clojure.data.json :as json]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.java.shell :as shell]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.handoff :as handoff]
   [task-conductor.agent-runner.hooks :as hooks])
  (:import
   [java.nio.file Files LinkOption]
   [java.nio.file.attribute PosixFilePermission]))

;;; Test Utilities

(defn with-temp-dir
  "Execute f with a temporary directory, cleaning up afterward."
  [f]
  (let [temp-dir (Files/createTempDirectory
                  "hooks-test"
                  (into-array java.nio.file.attribute.FileAttribute []))
        temp-path (.toString temp-dir)]
    (try
      (f temp-path)
      (finally
        ;; Clean up recursively
        (doseq [file (reverse (file-seq (io/file temp-path)))]
          (.delete file))))))

(defn file-executable?
  "Check if file has execute permission."
  [path]
  (let [link-opts (into-array LinkOption [])
        perms (Files/getPosixFilePermissions (.toPath (io/file path)) link-opts)]
    (contains? perms PosixFilePermission/OWNER_EXECUTE)))

;;; Hook Generation Tests

(deftest generate-stop-hook-test
  (testing "generate-stop-hook"
    (let [script (hooks/generate-stop-hook)]

      (testing "produces non-empty script"
        (is (not (str/blank? script))))

      (testing "starts with shebang"
        (is (str/starts-with? script "#!/bin/bash")))

      (testing "contains atomic write pattern"
        (is (str/includes? script "TEMP_FILE"))
        (is (str/includes? script "mv \"$TEMP_FILE\"")))

      (testing "writes EDN with :status and :timestamp"
        (is (str/includes? script ":status"))
        (is (str/includes? script ":timestamp")))

      (testing "exits with code 0"
        (is (str/includes? script "exit 0"))))))

(deftest generate-idle-hook-test
  (testing "generate-idle-hook"
    (let [script (hooks/generate-idle-hook)]

      (testing "produces non-empty script"
        (is (not (str/blank? script))))

      (testing "starts with shebang"
        (is (str/starts-with? script "#!/bin/bash")))

      (testing "contains atomic write pattern"
        (is (str/includes? script "TEMP_FILE"))
        (is (str/includes? script "mv \"$TEMP_FILE\"")))

      (testing "writes EDN with :status :idle and :source"
        (is (str/includes? script ":status :idle"))
        (is (str/includes? script ":timestamp"))
        (is (str/includes? script ":source :notification-hook")))

      (testing "exits with code 0"
        (is (str/includes? script "exit 0"))))))

(deftest generated-script-shellcheck-test
  (testing "generated stop script"
    (testing "passes shellcheck validation"
      (let [script (hooks/generate-stop-hook)
            result (shell/sh "shellcheck" "-" :in script)]
        (is (zero? (:exit result))
            (str "shellcheck errors: " (:out result) (:err result))))))

  (testing "generated idle script"
    (testing "passes shellcheck validation"
      (let [script (hooks/generate-idle-hook)
            result (shell/sh "shellcheck" "-" :in script)]
        (is (zero? (:exit result))
            (str "shellcheck errors: " (:out result) (:err result)))))))

(deftest generated-script-execution-test
  (testing "generated script execution"
    (with-temp-dir
      (fn [temp-dir]
        (testing "produces valid HookStatus EDN when run"
          (let [script (hooks/generate-stop-hook)
                script-file (io/file temp-dir "test-hook.sh")
                input-json "{\"session_id\": \"test-123\"}"
                ;; Write script
                _ (spit script-file script)
                ;; Make executable
                _ (shell/sh "chmod" "+x" (.getAbsolutePath script-file))
                ;; Run from temp-dir so .task-conductor is created there
                result (shell/sh "bash" (.getAbsolutePath script-file)
                                 :in input-json
                                 :dir temp-dir)
                handoff-file (io/file temp-dir ".task-conductor" "handoff.edn")]

            (is (zero? (:exit result))
                (str "script failed: " (:err result)))

            (is (.exists handoff-file)
                "handoff.edn should be created")

            (when (.exists handoff-file)
              (let [content (slurp handoff-file)
                    parsed (edn/read-string content)]
                (is (= :completed (:status parsed))
                    (str "Expected :completed status, got: " content))
                (is (string? (:timestamp parsed))
                    "timestamp should be a string")
                (is (re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z"
                                (:timestamp parsed))
                    "timestamp should be ISO-8601 format")))))))))

;;; Hook Installation Tests

(deftest install-stop-hook-test
  (testing "install-stop-hook"
    (with-temp-dir
      (fn [temp-dir]
        (testing "creates hook file in correct location"
          (let [hook-path (hooks/install-stop-hook temp-dir)
                expected-path (str temp-dir "/.task-conductor/hooks/stop-hook.sh")]
            (is (= expected-path hook-path))
            (is (.exists (io/file hook-path)))))

        (testing "creates executable file"
          (let [hook-path (hooks/install-stop-hook temp-dir)]
            (is (file-executable? hook-path))))

        (testing "file contains generated script content"
          (let [hook-path (hooks/install-stop-hook temp-dir)
                content (slurp hook-path)]
            (is (str/starts-with? content "#!/bin/bash"))))))))

;;; Settings Configuration Tests

(deftest configure-claude-settings-test
  (testing "configure-claude-settings"
    (with-temp-dir
      (fn [temp-dir]
        (testing "creates settings file when none exists"
          (hooks/configure-claude-settings temp-dir)
          (let [settings-file (io/file temp-dir ".claude" "settings.local.json")]
            (is (.exists settings-file))))

        (testing "creates valid JSON structure with Stop and Notification hooks"
          (hooks/configure-claude-settings temp-dir)
          (let [settings-file (io/file temp-dir ".claude" "settings.local.json")
                content (slurp settings-file)
                parsed (json/read-str content :key-fn keyword)]
            (is (map? parsed))
            (is (contains? parsed :hooks))
            (is (contains? (:hooks parsed) :Stop))
            (is (contains? (:hooks parsed) :Notification))))

        (testing "Stop hook has correct structure"
          (hooks/configure-claude-settings temp-dir)
          (let [settings-file (io/file temp-dir ".claude" "settings.local.json")
                parsed (json/read-str (slurp settings-file) :key-fn keyword)
                stop-hooks (get-in parsed [:hooks :Stop])
                first-hook (first stop-hooks)]
            (is (vector? stop-hooks))
            (is (= 1 (count stop-hooks)))
            (is (= ".task-conductor/hooks/stop-hook.sh"
                   (get-in first-hook [:hooks 0 :command])))))

        (testing "Notification hook has correct structure with matcher"
          (hooks/configure-claude-settings temp-dir)
          (let [settings-file (io/file temp-dir ".claude" "settings.local.json")
                parsed (json/read-str (slurp settings-file) :key-fn keyword)
                notification-hooks (get-in parsed [:hooks :Notification])
                first-hook (first notification-hooks)]
            (is (vector? notification-hooks))
            (is (= 1 (count notification-hooks)))
            (is (= "idle_prompt" (:matcher first-hook)))
            (is (= ".task-conductor/hooks/idle-hook.sh"
                   (get-in first-hook [:hooks 0 :command])))))

        (testing "preserves existing settings"
          ;; Create settings with existing content
          (let [settings-file (io/file temp-dir ".claude" "settings.local.json")]
            (.mkdirs (.getParentFile settings-file))
            (spit settings-file (json/write-str {:existing "value"})))
          ;; Configure hooks
          (hooks/configure-claude-settings temp-dir)
          ;; Check existing content preserved
          (let [parsed (json/read-str
                        (slurp (io/file temp-dir ".claude" "settings.local.json"))
                        :key-fn keyword)]
            (is (= "value" (:existing parsed)))))))))

;;; Idempotency Tests

(deftest ensure-hooks-installed-test
  (testing "ensure-hooks-installed"
    (with-temp-dir
      (fn [temp-dir]
        (testing "installs both hooks and configures settings on first call"
          (let [result (hooks/ensure-hooks-installed temp-dir)]
            (is (string? (:stop-hook-path result)))
            (is (string? (:idle-hook-path result)))
            (is (.exists (io/file (:stop-hook-path result))))
            (is (.exists (io/file (:idle-hook-path result))))
            (is (:settings-updated? result))))

        (testing "is idempotent on subsequent calls"
          ;; First call
          (hooks/ensure-hooks-installed temp-dir)
          ;; Second call should not update settings
          (let [result (hooks/ensure-hooks-installed temp-dir)]
            (is (false? (:settings-updated? result)))))

        (testing "returns correct hook paths"
          (let [result (hooks/ensure-hooks-installed temp-dir)]
            (is (str/ends-with? (:stop-hook-path result) "stop-hook.sh"))
            (is (str/ends-with? (:idle-hook-path result) "idle-hook.sh"))))))))

(deftest hook-installed?-test
  (testing "hook-installed?"
    (with-temp-dir
      (fn [temp-dir]
        (testing "returns false when no hooks installed"
          (is (false? (hooks/hook-installed? temp-dir))))

        (testing "returns false when only stop hook installed"
          (hooks/install-stop-hook temp-dir)
          (is (false? (hooks/hook-installed? temp-dir))))

        (testing "returns true after both hooks installed"
          (hooks/install-idle-hook temp-dir)
          (is (true? (hooks/hook-installed? temp-dir))))))))

(deftest settings-configured?-test
  (testing "settings-configured?"
    (with-temp-dir
      (fn [temp-dir]
        (testing "returns false when no settings"
          (is (false? (hooks/settings-configured? temp-dir))))

        (testing "returns false with empty settings"
          (let [settings-file (io/file temp-dir ".claude" "settings.local.json")]
            (.mkdirs (.getParentFile settings-file))
            (spit settings-file "{}"))
          (is (false? (hooks/settings-configured? temp-dir))))

        (testing "returns true after configuration"
          (hooks/configure-claude-settings temp-dir)
          (is (true? (hooks/settings-configured? temp-dir))))))))

;;; Integration Tests

(deftest full-hook-flow-test
  ;; Integration test exercising the complete hook workflow:
  ;; ensure-hooks-installed → execute hook → read-hook-status
  (testing "full hook flow"
    (with-temp-dir
      (fn [temp-dir]
        (testing "install, execute stop hook, and read status"
          (let [;; Step 1: Install hooks
                install-result (hooks/ensure-hooks-installed temp-dir)
                stop-hook-path (:stop-hook-path install-result)
                ;; The hook writes to .task-conductor/handoff.edn
                handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                ;; Simulate CLI input (JSON with session_id)
                input-json "{\"session_id\": \"test-session\"}"]

            (testing "ensure-hooks-installed creates stop hook file"
              (is (string? stop-hook-path))
              (is (.exists (io/file stop-hook-path)))
              (is (file-executable? stop-hook-path)))

            (testing "ensure-hooks-installed configures settings"
              (is (:settings-updated? install-result)))

            ;; Step 2: Execute the installed stop hook script
            (let [result (shell/sh "bash" stop-hook-path
                                   :in input-json
                                   :dir temp-dir)]
              (testing "stop hook script executes successfully"
                (is (zero? (:exit result))
                    (str "script failed: " (:err result))))

              (testing "stop hook script creates handoff file"
                (is (.exists (io/file handoff-path))
                    "handoff.edn should be created")))

            ;; Step 3: Read hook status via handoff/read-hook-status
            (let [hook-status (handoff/read-hook-status handoff-path)]
              (testing "read-hook-status returns valid HookStatus"
                (is (some? hook-status)
                    "read-hook-status should return non-nil")
                (is (handoff/valid-hook-status? hook-status)
                    "returned status should be valid"))

              (testing "HookStatus has expected values for stop hook"
                (is (= :completed (:status hook-status)))
                (is (inst? (:timestamp hook-status)))))))

        (testing "install, execute idle hook, and read status"
          (let [;; Use the already installed hooks
                install-result (hooks/ensure-hooks-installed temp-dir)
                idle-hook-path (:idle-hook-path install-result)
                handoff-path (str temp-dir "/.task-conductor/handoff.edn")
                input-json "{\"type\": \"notification\"}"]

            (testing "ensure-hooks-installed creates idle hook file"
              (is (string? idle-hook-path))
              (is (.exists (io/file idle-hook-path)))
              (is (file-executable? idle-hook-path)))

            ;; Execute the idle hook script
            (let [result (shell/sh "bash" idle-hook-path
                                   :in input-json
                                   :dir temp-dir)]
              (testing "idle hook script executes successfully"
                (is (zero? (:exit result))
                    (str "script failed: " (:err result)))))

            ;; Read hook status
            (let [hook-status (handoff/read-hook-status handoff-path)]
              (testing "HookStatus has expected values for idle hook"
                (is (= :idle (:status hook-status)))
                (is (= :notification-hook (:source hook-status)))
                (is (inst? (:timestamp hook-status)))))))))))
