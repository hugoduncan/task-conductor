(ns task-conductor.project.resolvers-test
  ;; Verify resolvers and mutations correctly delegate to registry.
  ;; Each test confirms the resolver/mutation calls the expected functions
  ;; and returns properly formatted results through Pathom.
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [deftest is testing]]
   [task-conductor.pathom-graph.interface :as graph]
   [task-conductor.project.registry :as registry]
   [task-conductor.project.resolvers :as resolvers]))

(defmacro with-clean-state
  "Execute body with clean registry and graph state."
  [& body]
  `(do
     (registry/clear!)
     (graph/reset-graph!)
     (resolvers/register-resolvers!)
     (try
       ~@body
       (finally
         (registry/clear!)
         (graph/reset-graph!)))))

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

;;; Resolver Tests

(deftest all-projects-test
  (with-clean-state
    (testing "all-projects"
      (testing "returns empty list when no projects registered"
        (let [result (graph/query [:project/all])]
          (is (= [] (:project/all result)))))

      (testing "returns all registered projects"
        (with-temp-dir [dir1]
          (with-temp-dir [dir2]
            (registry/register! dir1 {:project/name "p1"})
            (registry/register! dir2 {:project/name "p2"})
            (let [result (graph/query [:project/all])
                  projects (:project/all result)
                  by-name (into {} (map (juxt :project/name identity)) projects)]
              (is (= 2 (count projects)))
              (is (= dir1 (:project/path (get by-name "p1"))))
              (is (= dir2 (:project/path (get by-name "p2")))))))))))

(deftest project-by-path-test
  (with-clean-state
    (testing "project-by-path"
      (testing "returns project name for existing path"
        (with-temp-dir [dir]
          (registry/register! dir {:project/name "myproj"})
          (let [result (graph/query {:project/path dir} [:project/name])]
            (is (= "myproj" (:project/name result))))))

      (testing "returns empty map for non-existent path"
        (with-temp-dir [dir]
          (let [result (graph/query {:project/path dir} [:project/name])]
            (is (nil? (:project/name result)))))))))

(deftest project-by-name-test
  (with-clean-state
    (testing "project-by-name"
      (testing "returns project path for existing name"
        (with-temp-dir [dir]
          (registry/register! dir {:project/name "myproj"})
          (let [result (graph/query {:project/name "myproj"} [:project/path])]
            (is (= dir (:project/path result))))))

      (testing "returns empty map for non-existent name"
        (let [result (graph/query {:project/name "nosuch"} [:project/path])]
          (is (nil? (:project/path result))))))))

;;; Mutation Tests

(deftest project-create-test
  (with-clean-state
    (testing "project-create!"
      (testing "creates project with default name"
        (with-temp-dir [dir]
          (let [result (graph/query [`(resolvers/project-create!
                                       {:project/path ~dir})])
                project (get-in result [`resolvers/project-create! :project/result])]
            (is (= dir (:project/path project)))
            (is (string? (:project/name project))))))

      (testing "creates project with custom name"
        (with-temp-dir [dir]
          (let [result (graph/query [`(resolvers/project-create!
                                       {:project/path ~dir
                                        :project/name "custom"})])
                project (get-in result [`resolvers/project-create! :project/result])]
            (is (= dir (:project/path project)))
            (is (= "custom" (:project/name project))))))

      (testing "returns error for duplicate path"
        (with-temp-dir [dir]
          (registry/register! dir)
          (let [result (graph/query [`(resolvers/project-create!
                                       {:project/path ~dir})])
                error (get-in result [`resolvers/project-create! :project/result])]
            (is (= :duplicate-path (:error error))))))

      (testing "returns error for non-existent path"
        (let [result (graph/query [`(resolvers/project-create!
                                     {:project/path "/no/such/path"})])
              error (get-in result [`resolvers/project-create! :project/result])]
          (is (= :path-not-found (:error error))))))))

(deftest project-update-test
  (with-clean-state
    (testing "project-update!"
      (testing "updates project name"
        (with-temp-dir [dir]
          (registry/register! dir {:project/name "orig"})
          (let [result (graph/query [`(resolvers/project-update!
                                       {:project/path ~dir
                                        :project/name "renamed"})])
                project (get-in result [`resolvers/project-update! :project/result])]
            (is (= "renamed" (:project/name project)))
            (is (= dir (:project/path project))))))

      (testing "returns error for non-existent project"
        (with-temp-dir [dir]
          (let [result (graph/query [`(resolvers/project-update!
                                       {:project/path ~dir
                                        :project/name "x"})])
                error (get-in result [`resolvers/project-update! :project/result])]
            (is (= :project-not-found (:error error))))))

      (testing "returns error for duplicate name"
        (with-temp-dir [dir1]
          (with-temp-dir [dir2]
            (registry/register! dir1 {:project/name "taken"})
            (registry/register! dir2 {:project/name "other"})
            (let [result (graph/query [`(resolvers/project-update!
                                         {:project/path ~dir2
                                          :project/name "taken"})])
                  error (get-in result [`resolvers/project-update! :project/result])]
              (is (= :duplicate-name (:error error))))))))))

(deftest project-delete-test
  (with-clean-state
    (testing "project-delete!"
      (testing "deletes project and returns it"
        (with-temp-dir [dir]
          (let [project (registry/register! dir {:project/name "todel"})
                result (graph/query [`(resolvers/project-delete!
                                       {:project/path ~dir})])
                deleted (get-in result [`resolvers/project-delete! :project/result])]
            (is (= project deleted))
            (is (nil? (registry/get-by-path dir))))))

      (testing "returns nil for non-existent project"
        (with-temp-dir [dir]
          (let [result (graph/query [`(resolvers/project-delete!
                                       {:project/path ~dir})])
                deleted (get-in result [`resolvers/project-delete! :project/result])]
            (is (nil? deleted))))))))
