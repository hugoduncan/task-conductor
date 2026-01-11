(ns task-conductor.python-venv.core-test
  "Unit tests for python-venv core implementation."
  (:require
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
