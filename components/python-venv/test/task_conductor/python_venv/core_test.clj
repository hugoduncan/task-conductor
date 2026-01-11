(ns task-conductor.python-venv.core-test
  "Unit tests for python-venv core implementation."
  (:require
   [babashka.process :as p]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.python-venv.core :as core]))

(deftest resolve-python-cmd-test
  (testing "resolve-python-cmd"
    (testing "returns :python-cmd option when provided"
      (is (= "/usr/bin/python3.11"
             (core/resolve-python-cmd {:python-cmd "/usr/bin/python3.11"}))
          "should use the explicit :python-cmd option"))

    (testing "returns env var when option not provided"
      (with-redefs [core/get-env (fn [name]
                                   (when (= name "TASK_CONDUCTOR_PYTHON")
                                     "/custom/python"))]
        (is (= "/custom/python"
               (core/resolve-python-cmd {}))
            "should use TASK_CONDUCTOR_PYTHON env var when option is nil")))

    (testing "returns python3 as default when neither option nor env var is set"
      (with-redefs [core/get-env (constantly nil)]
        (is (= "python3"
               (core/resolve-python-cmd {}))
            "should default to python3 when no option or env var")))

    (testing "option takes precedence over env var"
      (with-redefs [core/get-env (fn [name]
                                   (when (= name "TASK_CONDUCTOR_PYTHON")
                                     "/env/python"))]
        (is (= "/option/python"
               (core/resolve-python-cmd {:python-cmd "/option/python"}))
            "should prefer :python-cmd option over env var")))))

(deftest ensure!-python-cmd-test
  (testing "ensure! uses :python-cmd option"
    (let [captured-cmd (atom nil)]
      (with-redefs [core/exists? (constantly false)
                    core/pip-path (constantly "/test-venv/bin/pip")
                    p/shell (fn [_opts & cmd-args]
                              ;; Capture the python command from the venv creation call
                              (when (some #(= "-m" %) cmd-args)
                                (reset! captured-cmd (first cmd-args)))
                              {:exit 0 :out "" :err ""})]
        (core/ensure! "/test-venv" "/test-requirements.txt"
                      {:python-cmd "/custom/python3.11" :quiet? true})
        (is (= "/custom/python3.11" @captured-cmd)
            "should use the :python-cmd option when creating venv"))))

  (testing "ensure! uses env var when :python-cmd not provided"
    (let [captured-cmd (atom nil)]
      (with-redefs [core/exists? (constantly false)
                    core/get-env (fn [name]
                                   (when (= name "TASK_CONDUCTOR_PYTHON")
                                     "/env/python"))
                    core/pip-path (constantly "/test-venv/bin/pip")
                    p/shell (fn [_opts & cmd-args]
                              (when (some #(= "-m" %) cmd-args)
                                (reset! captured-cmd (first cmd-args)))
                              {:exit 0 :out "" :err ""})]
        (core/ensure! "/test-venv" "/test-requirements.txt" {:quiet? true})
        (is (= "/env/python" @captured-cmd)
            "should use TASK_CONDUCTOR_PYTHON env var"))))

  (testing "ensure! defaults to python3 when neither option nor env var set"
    (let [captured-cmd (atom nil)]
      (with-redefs [core/exists? (constantly false)
                    core/get-env (constantly nil)
                    core/pip-path (constantly "/test-venv/bin/pip")
                    p/shell (fn [_opts & cmd-args]
                              (when (some #(= "-m" %) cmd-args)
                                (reset! captured-cmd (first cmd-args)))
                              {:exit 0 :out "" :err ""})]
        (core/ensure! "/test-venv" "/test-requirements.txt" {:quiet? true})
        (is (= "python3" @captured-cmd)
            "should default to python3")))))
