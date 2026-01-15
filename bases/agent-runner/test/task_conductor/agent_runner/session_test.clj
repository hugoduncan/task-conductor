(ns task-conductor.agent-runner.session-test
  ;; Unit tests for session namespace.
  ;;
  ;; Contracts tested:
  ;; - run-and-capture-session delegates to create-session-via-cli
  ;; - Returns session-id, empty messages vector, and response
  ;; - Uses :cwd from opts or defaults to current directory
  ;; - Passes :timeout-ms when provided
  ;; - Propagates CLI errors unchanged
  ;;
  ;; - run-cli-session creates CLI sessions for orchestrator use
  ;; - Returns {:result {:messages []} :session-id ...} format
  ;; - Uses :cwd from config or defaults to current directory
  ;; - Passes :timeout-ms when provided
  ;; - Propagates CLI errors unchanged
  (:require
   [clojure.test :refer [deftest is testing]]
   [task-conductor.agent-runner.cli :as cli]
   [task-conductor.agent-runner.session :as session]))

;;; run-and-capture-session Tests

(deftest run-and-capture-session-success-test
  (testing "run-and-capture-session"
    (testing "returns session-id, messages, and result"
      (with-redefs [cli/create-session-via-cli
                    (fn [_opts]
                      {:session-id "test-session-123"
                       :response {:session_id "test-session-123"
                                  :result "Hello!"}})]
        (let [result (session/run-and-capture-session "Hello")]
          (is (= "test-session-123" (:session-id result)))
          (is (= [] (:messages result)))
          (is (= {:session_id "test-session-123"
                  :result "Hello!"}
                 (:result result))))))

    (testing "passes prompt to CLI"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-and-capture-session "Test prompt")
          (is (= "Test prompt" (:prompt @captured-opts))))))

    (testing "uses :cwd from opts as working-dir"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-and-capture-session "Hi" {:cwd "/my/project"})
          (is (= "/my/project" (:working-dir @captured-opts))))))

    (testing "defaults working-dir to current directory"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-and-capture-session "Hi" {})
          (is (= (System/getProperty "user.dir")
                 (:working-dir @captured-opts))))))

    (testing "passes :timeout-ms when provided"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-and-capture-session "Hi" {:timeout-ms 5000})
          (is (= 5000 (:timeout-ms @captured-opts))))))

    (testing "omits :timeout-ms when not provided"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-and-capture-session "Hi" {})
          (is (not (contains? @captured-opts :timeout-ms))))))))

(deftest run-and-capture-session-error-test
  (testing "run-and-capture-session"
    (testing "propagates CLI timeout error"
      (with-redefs [cli/create-session-via-cli
                    (fn [_opts]
                      (throw (ex-info "CLI timed out"
                                      {:type :cli/timeout
                                       :timeout-ms 1000})))]
        (let [ex (try
                   (session/run-and-capture-session "Hi")
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/timeout (:type (ex-data ex)))))))

    (testing "propagates CLI non-zero-exit error"
      (with-redefs [cli/create-session-via-cli
                    (fn [_opts]
                      (throw (ex-info "CLI failed"
                                      {:type :cli/non-zero-exit
                                       :exit-code 1})))]
        (let [ex (try
                   (session/run-and-capture-session "Hi")
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/non-zero-exit (:type (ex-data ex)))))))

    (testing "propagates CLI parse error"
      (with-redefs [cli/create-session-via-cli
                    (fn [_opts]
                      (throw (ex-info "Parse failed"
                                      {:type :cli/parse-error})))]
        (let [ex (try
                   (session/run-and-capture-session "Hi")
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/parse-error (:type (ex-data ex)))))))

    (testing "propagates CLI missing-session-id error"
      (with-redefs [cli/create-session-via-cli
                    (fn [_opts]
                      (throw (ex-info "No session"
                                      {:type :cli/missing-session-id})))]
        (let [ex (try
                   (session/run-and-capture-session "Hi")
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/missing-session-id (:type (ex-data ex)))))))))

;;; run-cli-session Tests

(deftest run-cli-session-success-test
  (testing "run-cli-session"
    (testing "returns result with empty messages and session-id"
      (with-redefs [cli/create-session-via-cli
                    (fn [_opts]
                      {:session-id "cli-session-456"
                       :response {:session_id "cli-session-456"
                                  :result "Done!"}})]
        (let [result (session/run-cli-session {:cwd "/test"} "Hello")]
          (is (= "cli-session-456" (:session-id result)))
          (is (= [] (:messages result)))
          (is (= {:session_id "cli-session-456"
                  :result "Done!"}
                 (:result result))))))

    (testing "passes prompt to CLI"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-cli-session {} "Test prompt")
          (is (= "Test prompt" (:prompt @captured-opts))))))

    (testing "uses :cwd from config as working-dir"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-cli-session {:cwd "/my/project"} "Hi")
          (is (= "/my/project" (:working-dir @captured-opts))))))

    (testing "defaults working-dir to current directory"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-cli-session {} "Hi")
          (is (= (System/getProperty "user.dir")
                 (:working-dir @captured-opts))))))

    (testing "passes :timeout-ms when provided"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-cli-session {:timeout-ms 5000} "Hi")
          (is (= 5000 (:timeout-ms @captured-opts))))))

    (testing "omits :timeout-ms when not provided"
      (let [captured-opts (atom nil)]
        (with-redefs [cli/create-session-via-cli
                      (fn [opts]
                        (reset! captured-opts opts)
                        {:session-id "x" :response {}})]
          (session/run-cli-session {} "Hi")
          (is (not (contains? @captured-opts :timeout-ms))))))))

(deftest run-cli-session-error-test
  (testing "run-cli-session"
    (testing "propagates CLI timeout error"
      (with-redefs [cli/create-session-via-cli
                    (fn [_opts]
                      (throw (ex-info "CLI timed out"
                                      {:type :cli/timeout
                                       :timeout-ms 1000})))]
        (let [ex (try
                   (session/run-cli-session {} "Hi")
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/timeout (:type (ex-data ex)))))))

    (testing "propagates CLI non-zero-exit error"
      (with-redefs [cli/create-session-via-cli
                    (fn [_opts]
                      (throw (ex-info "CLI failed"
                                      {:type :cli/non-zero-exit
                                       :exit-code 1})))]
        (let [ex (try
                   (session/run-cli-session {} "Hi")
                   nil
                   (catch Exception e e))]
          (is (some? ex))
          (is (= :cli/non-zero-exit (:type (ex-data ex)))))))))
