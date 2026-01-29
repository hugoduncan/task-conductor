(ns task-conductor.workspace.core-test
  ;; Tests workspace state management, config reading, and external command parsing.
  ;; Contracts: state functions modify atom correctly, project-config handles
  ;; file states, project-worktrees parses porcelain format.
  (:require
   [babashka.process :as p]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.workspace.core :as core]))

(deftest set-focused-project!-test
  (testing "set-focused-project!"
    (testing "sets the focused project in state"
      (reset! core/workspace-state {:focused-project nil :projects #{}})
      (core/set-focused-project! "/test/project")
      (is (= "/test/project" (:focused-project @core/workspace-state))))))

(deftest focused-project-test
  (testing "focused-project"
    (testing "returns the currently focused project"
      (reset! core/workspace-state {:focused-project "/my/project" :projects #{}})
      (is (= "/my/project" (core/focused-project))))

    (testing "returns nil when no project is focused"
      (reset! core/workspace-state {:focused-project nil :projects #{}})
      (is (nil? (core/focused-project))))))

(deftest add-project!-test
  (testing "add-project!"
    (testing "adds a project to the projects set"
      (reset! core/workspace-state {:focused-project nil :projects #{}})
      (core/add-project! "/proj/a")
      (is (contains? (:projects @core/workspace-state) "/proj/a")))

    (testing "allows adding multiple projects"
      (reset! core/workspace-state {:focused-project nil :projects #{}})
      (core/add-project! "/proj/a")
      (core/add-project! "/proj/b")
      (is (= #{"/proj/a" "/proj/b"} (:projects @core/workspace-state))))))

(deftest remove-project!-test
  (testing "remove-project!"
    (testing "removes a project from the projects set"
      (reset! core/workspace-state {:focused-project nil :projects #{"/proj/a" "/proj/b"}})
      (core/remove-project! "/proj/a")
      (is (= #{"/proj/b"} (:projects @core/workspace-state))))

    (testing "does nothing when project not in set"
      (reset! core/workspace-state {:focused-project nil :projects #{"/proj/a"}})
      (core/remove-project! "/proj/nonexistent")
      (is (= #{"/proj/a"} (:projects @core/workspace-state))))))

(deftest list-projects-test
  (testing "list-projects"
    (testing "returns all projects in the workspace"
      (reset! core/workspace-state {:focused-project nil :projects #{"/a" "/b" "/c"}})
      (is (= #{"/a" "/b" "/c"} (core/list-projects))))

    (testing "returns empty set when no projects"
      (reset! core/workspace-state {:focused-project nil :projects #{}})
      (is (= #{} (core/list-projects))))))

(deftest project-config-test
  (testing "project-config"
    (testing "when config file exists and is valid"
      (let [temp-dir (System/getProperty "java.io.tmpdir")
            test-dir (str temp-dir "/workspace-test-" (System/currentTimeMillis))
            config-file (io/file test-dir ".task-conductor.edn")]
        (try
          (.mkdirs (io/file test-dir))
          (spit config-file "{:main-checkout \"/main\"}")
          (is (= {:ok {:main-checkout "/main"}}
                 (core/project-config test-dir)))
          (finally
            (.delete config-file)
            (.delete (io/file test-dir))))))

    (testing "when config file does not exist"
      (is (= {:ok nil}
             (core/project-config "/nonexistent/path"))))

    (testing "when config file contains invalid EDN"
      (let [temp-dir (System/getProperty "java.io.tmpdir")
            test-dir (str temp-dir "/workspace-test-" (System/currentTimeMillis))
            config-file (io/file test-dir ".task-conductor.edn")]
        (try
          (.mkdirs (io/file test-dir))
          (spit config-file "{invalid edn")
          (let [result (core/project-config test-dir)]
            (is (contains? result :error))
            (is (re-find #"Failed to parse" (:error result))))
          (finally
            (.delete config-file)
            (.delete (io/file test-dir))))))))

(deftest project-worktrees-test
  (testing "project-worktrees"
    (testing "parses porcelain output correctly"
      (let [porcelain-output (str "worktree /path/to/main\n"
                                  "HEAD abc123\n"
                                  "branch refs/heads/master\n"
                                  "\n"
                                  "worktree /path/to/feature\n"
                                  "HEAD def456\n"
                                  "branch refs/heads/feature-x\n")]
        (with-redefs [core/project-config (constantly {:ok nil})
                      p/sh (fn [_cmd _opts]
                             {:exit 0
                              :out porcelain-output
                              :err ""})]
          (let [result (core/project-worktrees "/test")]
            (is (= {:ok [{:path "/path/to/main"
                          :commit "abc123"
                          :branch "master"
                          :bare? false}
                         {:path "/path/to/feature"
                          :commit "def456"
                          :branch "feature-x"
                          :bare? false}]}
                   result))))))

    (testing "handles bare worktrees"
      (let [porcelain-output (str "worktree /path/to/bare\n"
                                  "HEAD abc123\n"
                                  "bare\n")]
        (with-redefs [core/project-config (constantly {:ok nil})
                      p/sh (fn [_cmd _opts]
                             {:exit 0
                              :out porcelain-output
                              :err ""})]
          (let [result (core/project-worktrees "/test")]
            (is (= true (-> result :ok first :bare?)))))))

    (testing "uses main-checkout from config when present"
      (let [captured-dir (atom nil)]
        (with-redefs [core/project-config (constantly {:ok {:main-checkout "/main"}})
                      p/sh (fn [_cmd opts]
                             (reset! captured-dir (:dir opts))
                             {:exit 0
                              :out "worktree /main\nHEAD abc\nbranch refs/heads/m\n"
                              :err ""})]
          (core/project-worktrees "/worktree")
          (is (= "/main" @captured-dir)))))

    (testing "returns error on git command failure"
      (with-redefs [core/project-config (constantly {:ok nil})
                    p/sh (fn [_cmd _opts]
                           {:exit 1
                            :out ""
                            :err "fatal: error"})]
        (let [result (core/project-worktrees "/test")]
          (is (contains? result :error)))))))

(deftest project-tasks-test
  (testing "project-tasks"
    (testing "parses mcp-tasks output correctly"
      (with-redefs [p/sh (fn [_cmd _opts]
                           {:exit 0
                            :out "[{:id 1 :title \"Task\"}]"
                            :err ""})]
        (let [result (core/project-tasks "/test")]
          (is (= {:ok [{:id 1 :title "Task"}]} result)))))

    (testing "returns error on command failure"
      (with-redefs [p/sh (fn [_cmd _opts]
                           {:exit 1
                            :out ""
                            :err "command not found"})]
        (let [result (core/project-tasks "/test")]
          (is (contains? result :error)))))

    (testing "returns error on invalid EDN output"
      (with-redefs [p/sh (fn [_cmd _opts]
                           {:exit 0
                            :out "[{:unclosed"
                            :err ""})]
        (let [result (core/project-tasks "/test")]
          (is (contains? result :error))
          (is (re-find #"Failed to parse" (:error result))))))))
