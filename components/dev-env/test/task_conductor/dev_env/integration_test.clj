(ns task-conductor.dev-env.integration-test
  ;; End-to-end integration tests for Emacs dev-env.
  ;; Verifies the full round-trip: Clojure -> Emacs socket server -> Clojure callback.
  ;; Runs Emacs in batch mode with mocked claude-code.el functions.
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is testing use-fixtures]]
   [task-conductor.dev-env.emacs :as emacs]
   [task-conductor.dev-env.interface :as dev-env])
  (:import
   [java.io BufferedReader InputStreamReader]
   [java.util.concurrent CountDownLatch TimeUnit]))

;;; Test Configuration

(def ^:private test-socket-path
  "Socket path for integration tests."
  "/tmp/task-conductor-integration-test.sock")

(def ^:private emacs-startup-timeout-ms
  "Timeout for Emacs to start and print READY."
  10000)

(def ^:private callback-timeout-ms
  "Timeout for callback to be invoked."
  5000)

;;; Emacs Process Management

(defn- find-emacs-dir
  "Find the emacs directory from current working directory."
  []
  (let [cwd (System/getProperty "user.dir")
        ;; Try direct path first (when running from project root)
        emacs-dir (io/file cwd "emacs")]
    (if (.exists emacs-dir)
      (.getAbsolutePath emacs-dir)
      ;; Try parent directories in case running from a subdirectory
      (loop [dir (io/file cwd)]
        (when dir
          (let [candidate (io/file dir "emacs")]
            (if (and (.exists candidate)
                     (.exists (io/file candidate "task-conductor-dev-env.el")))
              (.getAbsolutePath candidate)
              (recur (.getParentFile dir)))))))))

(defn- find-elpa-packages
  "Find elpa package directories for given package prefixes.
   Returns a sequence of directory paths."
  [& prefixes]
  (let [elpa-dir (io/file (System/getProperty "user.home") ".emacs.d" "elpa")]
    (when (.exists elpa-dir)
      (for [prefix prefixes
            :let [matches (->> (.listFiles elpa-dir)
                               (filter #(.isDirectory %))
                               (filter #(.startsWith (.getName %) prefix))
                               (sort-by #(.getName %) #(compare %2 %1)))]
            :when (seq matches)]
        (.getAbsolutePath (first matches))))))

(defn- start-emacs-server
  "Start Emacs in batch mode with integration test helper.
   Returns {:process Process, :ready-latch CountDownLatch, :output-lines atom}
   or nil if startup fails."
  []
  (let [emacs-dir (find-emacs-dir)]
    (when-not emacs-dir
      (throw (ex-info "Could not find emacs directory" {})))
    (let [helper-path (str emacs-dir "/test/integration-test-helper.el")
          ;; Find elpa packages for parseedn and its dependency parseclj
          elpa-paths (find-elpa-packages "parseedn-" "parseclj-")
          load-path-args (vec (mapcat (fn [p] ["-L" p]) elpa-paths))
          base-args ["emacs" "--batch"
                     "-L" emacs-dir
                     "-L" (str emacs-dir "/test")]
          load-args ["-l" "parseedn"
                     "-l" "task-conductor-dev-env"
                     "-l" helper-path
                     "-f" "integration-test-run"]
          all-args (vec (concat base-args load-path-args load-args))
          process-builder (ProcessBuilder. all-args)
          _ (.put (.environment process-builder) "TEST_SOCKET_PATH" test-socket-path)
          _ (.redirectErrorStream process-builder true)
          process (.start process-builder)
          reader (BufferedReader. (InputStreamReader. (.getInputStream process)))
          ready-latch (CountDownLatch. 1)
          output-lines (atom [])
          reader-thread (Thread.
                         (fn []
                           (try
                             (loop []
                               (when-let [line (.readLine reader)]
                                 (swap! output-lines conj line)
                                 (when (= "READY" (str/trim line))
                                   (.countDown ready-latch))
                                 (recur)))
                             (catch Exception _))))]
      (.setDaemon reader-thread true)
      (.start reader-thread)
      {:process process
       :ready-latch ready-latch
       :output-lines output-lines
       :reader-thread reader-thread})))

(defn- stop-emacs-server
  "Stop the Emacs server process."
  [{:keys [process reader-thread]}]
  (when process
    (.destroy process)
    (try
      (.waitFor process 5 TimeUnit/SECONDS)
      (catch InterruptedException _))
    (when (.isAlive process)
      (.destroyForcibly process)))
  (when reader-thread
    (.interrupt reader-thread)))

(defn- wait-for-ready
  "Wait for Emacs server to be ready. Returns true if ready, false if timeout."
  [{:keys [ready-latch]} timeout-ms]
  (.await ready-latch timeout-ms TimeUnit/MILLISECONDS))

(defn- cleanup-socket
  "Remove socket file if it exists."
  []
  (let [socket-file (io/file test-socket-path)]
    (when (.exists socket-file)
      (.delete socket-file))))

;;; Test Fixture

(def ^:dynamic *emacs-server* nil)

(defn emacs-server-fixture
  "Fixture that starts and stops Emacs server for each test."
  [f]
  (cleanup-socket)
  (let [server (start-emacs-server)]
    (try
      (if (wait-for-ready server emacs-startup-timeout-ms)
        (binding [*emacs-server* server]
          (f))
        (throw (ex-info "Emacs server failed to start"
                        {:output @(:output-lines server)})))
      (finally
        (stop-emacs-server server)
        (cleanup-socket)))))

(use-fixtures :each emacs-server-fixture)

;;; Tests

(deftest emacs-integration-round-trip-test
  (testing "Emacs integration"
    (testing "round-trip open-session to session-complete"
      (let [env (emacs/create-emacs-dev-env test-socket-path)
            result-latch (CountDownLatch. 1)
            result-atom (atom nil)]
        (is (some? env) "Should connect to Emacs socket server")
        (when env
          (try
            (dev-env/open-cli-session
             env
             {:session-id "integration-test-1"
              :prompt "test prompt"
              :working-dir "/tmp"}
             (fn [result]
               (reset! result-atom result)
               (.countDown result-latch)))

            (let [received? (.await result-latch callback-timeout-ms TimeUnit/MILLISECONDS)]
              (is received? "Callback should be invoked within timeout")
              (when received?
                (let [result @result-atom]
                  (is (= "integration-test-1" (:session-id result)))
                  (is (= :completed (:status result)))
                  (is (= 0 (:exit-code result)))
                  (is (some? (:hook-status result))))))
            (finally
              (emacs/stop! env))))))))

(deftest emacs-integration-multiple-sessions-test
  (testing "Emacs integration"
    (testing "handles multiple sequential sessions"
      (let [env (emacs/create-emacs-dev-env test-socket-path)]
        (is (some? env) "Should connect to Emacs socket server")
        (when env
          (try
            ;; First session
            (let [latch1 (CountDownLatch. 1)
                  result1 (atom nil)]
              (dev-env/open-cli-session
               env
               {:session-id "multi-test-1"
                :working-dir "/tmp"}
               (fn [r]
                 (reset! result1 r)
                 (.countDown latch1)))
              (is (.await latch1 callback-timeout-ms TimeUnit/MILLISECONDS)
                  "First session callback should be invoked")
              (is (= :completed (:status @result1))))

            ;; Second session
            (let [latch2 (CountDownLatch. 1)
                  result2 (atom nil)]
              (dev-env/open-cli-session
               env
               {:session-id "multi-test-2"
                :working-dir "/tmp"}
               (fn [r]
                 (reset! result2 r)
                 (.countDown latch2)))
              (is (.await latch2 callback-timeout-ms TimeUnit/MILLISECONDS)
                  "Second session callback should be invoked")
              (is (= :completed (:status @result2))))
            (finally
              (emacs/stop! env))))))))

(deftest emacs-integration-close-session-test
  (testing "Emacs integration"
    (testing "close-session cancels pending session"
      (let [env (emacs/create-emacs-dev-env test-socket-path)]
        (is (some? env) "Should connect to Emacs socket server")
        (when env
          (try
            (let [latch (CountDownLatch. 1)
                  result (atom nil)]
              ;; Open session
              (dev-env/open-cli-session
               env
               {:session-id "close-test-1"
                :working-dir "/tmp"}
               (fn [r]
                 (reset! result r)
                 (.countDown latch)))
              ;; Immediately close it
              (dev-env/close-session env "close-test-1")
              ;; Callback should be invoked with :cancelled
              (is (.await latch callback-timeout-ms TimeUnit/MILLISECONDS)
                  "Callback should be invoked")
              (is (= :cancelled (:status @result))))
            (finally
              (emacs/stop! env))))))))
