(ns task-conductor.project.registry-test
  ;; Verify registry functions correctly manage project entries,
  ;; including registration lifecycle, path canonicalization, name uniqueness,
  ;; and error handling for invalid paths.
  (:require
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

(defmacro with-temp-dir
  "Execute body with a temporary directory bound to sym."
  [[sym] & body]
  `(let [dir# (io/file (System/getProperty "java.io.tmpdir")
                       (str "project-test-" (System/nanoTime)))
         ~sym (.getCanonicalPath dir#)]
     (.mkdirs dir#)
     (try
       ~@body
       (finally
         (.delete dir#)))))

(defmacro with-temp-file
  "Execute body with a temporary file (not directory) bound to sym."
  [[sym] & body]
  `(let [file# (io/file (System/getProperty "java.io.tmpdir")
                        (str "project-test-file-" (System/nanoTime)))
         ~sym (.getCanonicalPath file#)]
     (.createNewFile file#)
     (try
       ~@body
       (finally
         (.delete file#)))))

;;; Path utilities

(deftest canonicalize-path-test
  (testing "canonicalize-path"
    (testing "returns absolute path"
      (let [result (registry/canonicalize-path ".")]
        (is (string? result))
        (is (.isAbsolute (io/file result)))))

    (testing "resolves relative paths"
      (with-temp-dir [dir]
        (let [relative (str dir "/../" (.getName (io/file dir)))
              canonical (registry/canonicalize-path relative)]
          (is (= dir canonical)))))))

(deftest validate-path-test
  (testing "validate-path"
    (testing "returns nil for existing directory"
      (with-temp-dir [dir]
        (is (nil? (registry/validate-path dir)))))

    (testing "returns :path-not-found error for non-existent path"
      (let [result (registry/validate-path "/nonexistent/path")]
        (is (= :path-not-found (:error result)))
        (is (string? (:message result)))))

    (testing "returns :not-a-directory error for file path"
      (with-temp-file [file]
        (let [result (registry/validate-path file)]
          (is (= :not-a-directory (:error result)))
          (is (string? (:message result))))))))

;;; Registration

(deftest register-test
  (with-clean-project-state
    (testing "register!"
      (testing "creates project with canonical path"
        (with-temp-dir [dir]
          (let [project (registry/register! dir)]
            (is (map? project))
            (is (= dir (:project/path project))))))

      (testing "defaults name to final path segment"
        (with-temp-dir [dir]
          (let [project (registry/register! dir)
                expected-name (.getName (io/file dir))]
            (is (= expected-name (:project/name project))))))

      (testing "accepts custom name via opts"
        (with-temp-dir [dir]
          (let [project (registry/register! dir {:project/name "custom"})]
            (is (= "custom" (:project/name project))))))

      (testing "returns :duplicate-path error for same path"
        (with-temp-dir [dir]
          (registry/register! dir)
          (let [result (registry/register! dir)]
            (is (= :duplicate-path (:error result))))))

      (testing "returns :duplicate-name error for same name"
        (with-temp-dir [dir1]
          (with-temp-dir [dir2]
            (registry/register! dir1 {:project/name "shared"})
            (let [result (registry/register! dir2 {:project/name "shared"})]
              (is (= :duplicate-name (:error result)))))))

      (testing "returns :path-not-found error for non-existent path"
        (let [result (registry/register! "/nonexistent/path")]
          (is (= :path-not-found (:error result)))))

      (testing "returns :not-a-directory error for file"
        (with-temp-file [file]
          (let [result (registry/register! file)]
            (is (= :not-a-directory (:error result)))))))))

;;; Unregistration

(deftest unregister-test
  (with-clean-project-state
    (testing "unregister!"
      (testing "removes project and returns it"
        (with-temp-dir [dir]
          (let [project (registry/register! dir)
                removed (registry/unregister! dir)]
            (is (= project removed))
            (is (nil? (registry/get-by-path dir))))))

      (testing "returns nil for non-existent project"
        (with-temp-dir [dir]
          (is (nil? (registry/unregister! dir))))))))

;;; Update

(deftest update-test
  (with-clean-project-state
    (testing "update!"
      (testing "merges updates into existing project"
        (with-temp-dir [dir]
          (registry/register! dir {:project/name "original"})
          (let [updated (registry/update! dir {:project/name "renamed"})]
            (is (= "renamed" (:project/name updated)))
            (is (= dir (:project/path updated))))))

      (testing "allows updating to same name"
        (with-temp-dir [dir]
          (registry/register! dir {:project/name "same"})
          (let [updated (registry/update! dir {:project/name "same"})]
            (is (= "same" (:project/name updated))))))

      (testing "returns :duplicate-name error when changing to existing name"
        (with-temp-dir [dir1]
          (with-temp-dir [dir2]
            (registry/register! dir1 {:project/name "taken"})
            (registry/register! dir2 {:project/name "other"})
            (let [result (registry/update! dir2 {:project/name "taken"})]
              (is (= :duplicate-name (:error result)))))))

      (testing "returns :project-not-found error for non-existent project"
        (with-temp-dir [dir]
          (let [result (registry/update! dir {:project/name "new"})]
            (is (= :project-not-found (:error result)))))))))

;;; Lookup

(deftest get-by-path-test
  (with-clean-project-state
    (testing "get-by-path"
      (testing "returns project for existing path"
        (with-temp-dir [dir]
          (let [project (registry/register! dir)]
            (is (= project (registry/get-by-path dir))))))

      (testing "returns nil for non-existent path"
        (with-temp-dir [dir]
          (is (nil? (registry/get-by-path dir))))))))

(deftest get-by-name-test
  (with-clean-project-state
    (testing "get-by-name"
      (testing "returns project for existing name"
        (with-temp-dir [dir]
          (let [project (registry/register! dir {:project/name "myproject"})]
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
        (with-temp-dir [dir1]
          (with-temp-dir [dir2]
            (let [p1 (registry/register! dir1 {:project/name "proj1"})
                  p2 (registry/register! dir2 {:project/name "proj2"})
                  listed (registry/list-all)]
              (is (= 2 (count listed)))
              (is (= #{p1 p2} (set listed))))))))))

;;; Clear

(deftest clear-test
  (with-clean-project-state
    (testing "clear!"
      (testing "removes all entries from registry"
        (with-temp-dir [dir1]
          (with-temp-dir [dir2]
            (registry/register! dir1)
            (registry/register! dir2)
            (is (= 2 (count (registry/list-all))))
            (registry/clear!)
            (is (= [] (registry/list-all)))))))))
