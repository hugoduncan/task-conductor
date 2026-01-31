(ns task-conductor.project.registry-test
  ;; Verify registry functions correctly manage project entries,
  ;; including registration lifecycle, path canonicalization, name uniqueness,
  ;; and error handling for invalid paths.
  (:require
   [babashka.fs :as fs]
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.project.registry :as registry]))

(defmacro with-clean-project-state
  "Execute body with a clean registry, ensuring cleanup afterwards."
  [& body]
  `(do
     (registry/clear!)
     (try
       ~@body
       (finally
         (registry/clear!)))))

;;; Path utilities

(deftest canonicalize-path-test
  (testing "canonicalize-path"
    (testing "returns absolute path"
      (let [result (registry/canonicalize-path ".")]
        (is (string? result))
        (is (.isAbsolute (io/file result)))))

    (testing "resolves relative paths"
      (fs/with-temp-dir [tmp]
        (let [dir (str (fs/canonicalize tmp))
              relative (str dir "/../" (fs/file-name tmp))
              canonical (registry/canonicalize-path relative)]
          (is (= dir canonical)))))))

(deftest validate-path-test
  (testing "validate-path"
    (testing "returns nil for existing directory"
      (fs/with-temp-dir [tmp]
        (is (nil? (registry/validate-path (str tmp))))))

    (testing "returns :path-not-found error for non-existent path"
      (let [result (registry/validate-path "/nonexistent/path")]
        (is (= :path-not-found (:error result)))
        (is (string? (:message result)))))

    (testing "returns :not-a-directory error for file path"
      (let [tmp (fs/create-temp-file)]
        (try
          (let [result (registry/validate-path (str tmp))]
            (is (= :not-a-directory (:error result)))
            (is (string? (:message result))))
          (finally
            (fs/delete-if-exists tmp)))))))

;;; Registration

(deftest register-test
  (with-clean-project-state
    (testing "register!"
      (testing "creates project with canonical path"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                project (registry/register! dir)]
            (is (map? project))
            (is (= dir (:project/path project))))))

      (testing "defaults name to final path segment"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                project (registry/register! dir)
                expected-name (str (fs/file-name tmp))]
            (is (= expected-name (:project/name project))))))

      (testing "accepts custom name via opts"
        (fs/with-temp-dir [tmp]
          (let [project (registry/register! (str tmp) {:project/name "custom"})]
            (is (= "custom" (:project/name project))))))

      (testing "returns :duplicate-path error for same path"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))]
            (registry/register! dir)
            (let [result (registry/register! dir)]
              (is (= :duplicate-path (:error result)))))))

      (testing "returns :duplicate-name error for same name"
        (fs/with-temp-dir [tmp1]
          (fs/with-temp-dir [tmp2]
            (registry/register! (str tmp1) {:project/name "shared"})
            (let [result (registry/register! (str tmp2) {:project/name "shared"})]
              (is (= :duplicate-name (:error result)))))))

      (testing "returns :path-not-found error for non-existent path"
        (let [result (registry/register! "/nonexistent/path")]
          (is (= :path-not-found (:error result)))))

      (testing "returns :not-a-directory error for file"
        (let [tmp (fs/create-temp-file)]
          (try
            (let [result (registry/register! (str tmp))]
              (is (= :not-a-directory (:error result))))
            (finally
              (fs/delete-if-exists tmp))))))))

;;; Unregistration

(deftest unregister-test
  (with-clean-project-state
    (testing "unregister!"
      (testing "removes project and returns it"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                project (registry/register! dir)
                removed (registry/unregister! dir)]
            (is (= project removed))
            (is (nil? (registry/get-by-path dir))))))

      (testing "returns nil for non-existent project"
        (fs/with-temp-dir [tmp]
          (is (nil? (registry/unregister! (str tmp)))))))))

;;; Update

(deftest update-test
  (with-clean-project-state
    (testing "update!"
      (testing "merges updates into existing project"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))]
            (registry/register! dir {:project/name "original"})
            (let [updated (registry/update! dir {:project/name "renamed"})]
              (is (= "renamed" (:project/name updated)))
              (is (= dir (:project/path updated)))))))

      (testing "allows updating to same name"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))]
            (registry/register! dir {:project/name "same"})
            (let [updated (registry/update! dir {:project/name "same"})]
              (is (= "same" (:project/name updated)))))))

      (testing "returns :duplicate-name error when changing to existing name"
        (fs/with-temp-dir [tmp1]
          (fs/with-temp-dir [tmp2]
            (let [dir1 (str tmp1)
                  dir2 (str tmp2)]
              (registry/register! dir1 {:project/name "taken"})
              (registry/register! dir2 {:project/name "other"})
              (let [result (registry/update! dir2 {:project/name "taken"})]
                (is (= :duplicate-name (:error result))))))))

      (testing "returns :project-not-found error for non-existent project"
        (fs/with-temp-dir [tmp]
          (let [result (registry/update! (str tmp) {:project/name "new"})]
            (is (= :project-not-found (:error result))))))

      (testing "ignores keys other than :project/name"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))]
            (registry/register! dir {:project/name "orig"})
            (let [updated (registry/update! dir {:project/name "new"
                                                 :project/path "/other"
                                                 :unexpected "value"})]
              (is (= "new" (:project/name updated)))
              (is (= dir (:project/path updated)))
              (is (nil? (:unexpected updated)))))))

      (testing "returns existing project unchanged for empty updates"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                project (registry/register! dir {:project/name "orig"})
                result (registry/update! dir {})]
            (is (= project result)))))

      (testing "returns :project-not-found for empty updates on missing project"
        (fs/with-temp-dir [tmp]
          (let [result (registry/update! (str tmp) {})]
            (is (= :project-not-found (:error result)))))))))

;;; Lookup

(deftest get-by-path-test
  (with-clean-project-state
    (testing "get-by-path"
      (testing "returns project for existing path"
        (fs/with-temp-dir [tmp]
          (let [dir (str (fs/canonicalize tmp))
                project (registry/register! dir)]
            (is (= project (registry/get-by-path dir))))))

      (testing "returns nil for non-existent path"
        (fs/with-temp-dir [tmp]
          (is (nil? (registry/get-by-path (str tmp)))))))))

(deftest get-by-name-test
  (with-clean-project-state
    (testing "get-by-name"
      (testing "returns project for existing name"
        (fs/with-temp-dir [tmp]
          (let [project (registry/register! (str tmp) {:project/name "myproject"})]
            (is (= project (registry/get-by-name "myproject"))))))

      (testing "returns nil for non-existent name"
        (is (nil? (registry/get-by-name "nonexistent")))))))

;;; Listing

(deftest list-all-test
  (with-clean-project-state
    (testing "list-all"
      (testing "returns empty vector when registry is empty"
        (is (= [] (registry/list-all))))

      (testing "returns all registered projects"
        (fs/with-temp-dir [tmp1]
          (fs/with-temp-dir [tmp2]
            (let [p1 (registry/register! (str tmp1) {:project/name "proj1"})
                  p2 (registry/register! (str tmp2) {:project/name "proj2"})
                  listed (registry/list-all)]
              (is (= 2 (count listed)))
              (is (= #{p1 p2} (set listed))))))))))

;;; Clear

(deftest clear-test
  (with-clean-project-state
    (testing "clear!"
      (testing "removes all entries from registry"
        (fs/with-temp-dir [tmp1]
          (fs/with-temp-dir [tmp2]
            (registry/register! (str tmp1))
            (registry/register! (str tmp2))
            (is (= 2 (count (registry/list-all))))
            (registry/clear!)
            (is (= [] (registry/list-all)))))))))
