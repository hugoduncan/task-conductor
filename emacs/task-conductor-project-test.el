;;; task-conductor-project-test.el --- Tests for task-conductor-project -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for the project buffer rendering logic.

;;; Code:

(require 'ert)
(require 'task-conductor-project)

(defmacro with-project-buffer (&rest body)
  "Execute BODY in a temporary project buffer with project mode active."
  (declare (indent 0))
  `(with-temp-buffer
     (task-conductor-project-mode)
     ,@body))

(ert-deftest task-conductor-project-render-empty ()
  ;; Rendering with no projects shows the header and "(none)".
  (with-project-buffer
    (task-conductor-project--render nil)
    (let ((text (buffer-string)))
      (should (string-match-p "Projects (0)" text))
      (should (string-match-p "(none)" text)))))

(ert-deftest task-conductor-project-render-single ()
  ;; Rendering a single project shows name and path.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "myproj" :project/path "/tmp/myproj")))
    (let ((text (buffer-string)))
      (should (string-match-p "Projects (1)" text))
      (should (string-match-p "myproj" text))
      (should (string-match-p "/tmp/myproj" text)))))

(ert-deftest task-conductor-project-render-multiple ()
  ;; Rendering multiple projects aligns names by padding.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "ab" :project/path "/a")
           (list :project/name "longname" :project/path "/b")))
    (let ((text (buffer-string)))
      (should (string-match-p "Projects (2)" text))
      (should (string-match-p "ab" text))
      (should (string-match-p "longname" text)))))

(ert-deftest task-conductor-project-format-entry-padding ()
  ;; Format pads name to the given width with status icon prefix.
  (let ((entry (task-conductor-project--format-entry
                (list :project/name "ab" :project/path "/x")
                10)))
    (should (string-match-p "ab " entry))
    (should (string-match-p "/x" entry))))

(ert-deftest task-conductor-project-max-name-width ()
  ;; Max width is computed from the longest name.
  (should (= 8 (task-conductor-project--max-name-width
                (list (list :project/name "ab")
                      (list :project/name "longname"))))))

(ert-deftest task-conductor-project-render-section-values ()
  ;; Each project entry section stores the project plist as its value.
  (with-project-buffer
    (let* ((proj (list :project/name "test" :project/path "/t"))
           (projects (list proj)))
      (task-conductor-project--render projects)
      (goto-char (point-min))
      ;; Navigate into the first entry section
      (magit-section-forward)
      (let ((section (magit-current-section)))
        (should (eq (oref section type) 'task-conductor-project-entry))
        (should (equal "test" (plist-get (oref section value)
                                         :project/name)))))))

;;; Status display tests

(ert-deftest task-conductor-project-status-icon ()
  ;; Status icons map correctly.
  (should (equal "⚡" (task-conductor-project--status-icon :running)))
  (should (equal "⚡" (task-conductor-project--status-icon :escalated)))
  (should (equal "⏸" (task-conductor-project--status-icon :idle)))
  (should (equal " " (task-conductor-project--status-icon nil))))

(ert-deftest task-conductor-project-status-info-single ()
  ;; Status info shows state and task ID for a single session.
  (let* ((sessions (list (list :state :running :task-id 42)))
         (project (list :project/name "p" :project/path "/p"
                        :project/status :running
                        :project/active-sessions sessions))
         (info (task-conductor-project--status-info project)))
    (should (string-match-p "running" info))
    (should (string-match-p "task 42" info))))

(ert-deftest task-conductor-project-status-info-multiple ()
  ;; Status info shows count for multiple sessions.
  (let* ((sessions (list (list :state :running :task-id 1)
                         (list :state :idle :task-id 2)))
         (project (list :project/name "p" :project/path "/p"
                        :project/active-sessions sessions))
         (info (task-conductor-project--status-info project)))
    (should (string-match-p "\\+1" info))))

(ert-deftest task-conductor-project-status-info-nil ()
  ;; No status info when no sessions.
  (let ((project (list :project/name "p" :project/path "/p")))
    (should-not (task-conductor-project--status-info project))))

(ert-deftest task-conductor-project-format-entry-with-status ()
  ;; Format includes status icon and session info.
  (let* ((sessions (list (list :state :running :task-id 5)))
         (project (list :project/name "proj" :project/path "/proj"
                        :project/status :running
                        :project/active-sessions sessions))
         (entry (task-conductor-project--format-entry project 10)))
    (should (string-match-p "⚡" entry))
    (should (string-match-p "running: task 5" entry))))

(ert-deftest task-conductor-project-format-entry-no-status ()
  ;; Format shows space icon when no status.
  (let* ((project (list :project/name "proj" :project/path "/proj"))
         (entry (task-conductor-project--format-entry project 10)))
    (should-not (string-match-p "⚡" entry))
    (should-not (string-match-p "⏸" entry))))

(ert-deftest task-conductor-project-rerender-if-live ()
  ;; Rerender updates buffer from cached projects when buffer exists.
  (let* ((buf-name "*tc-test-proj-rerender*")
         (task-conductor-project--buffer-name buf-name)
         (task-conductor-dev-env--cached-projects
          (list (list :project/name "cached" :project/path "/cached")))
         (buf (get-buffer-create buf-name)))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (task-conductor-project-mode))
          (task-conductor-project-rerender-if-live)
          (with-current-buffer buf
            (should (string-match-p "cached" (buffer-string)))
            (should (string-match-p "Projects (1)" (buffer-string)))))
      (kill-buffer buf))))

;;; Error handling tests

(ert-deftest task-conductor-project-eval-or-error-nil-result ()
  ;; Returns error plist when eval-sync returns nil.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t))
            ((symbol-function 'task-conductor-dev-env--eval-sync)
             (lambda (_form) nil)))
    (let ((result (task-conductor-project--eval-or-error "(foo)")))
      (should (eq :error (plist-get result :status)))
      (should (plist-get result :message)))))

(ert-deftest task-conductor-project-eval-or-error-nrepl-error ()
  ;; Returns error plist when eval-sync signals an error.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t))
            ((symbol-function 'task-conductor-dev-env--eval-sync)
             (lambda (_form) (error "nREPL timeout"))))
    (let ((result (task-conductor-project--eval-or-error "(foo)")))
      (should (eq :error (plist-get result :status)))
      (should (string-match-p "timeout" (plist-get result :message))))))

(ert-deftest task-conductor-project-eval-or-error-success ()
  ;; Returns the eval-sync result when successful.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t))
            ((symbol-function 'task-conductor-dev-env--eval-sync)
             (lambda (_form) (list :status :ok :projects nil))))
    (let ((result (task-conductor-project--eval-or-error "(foo)")))
      (should (eq :ok (plist-get result :status))))))

(ert-deftest task-conductor-project-eval-or-error-not-connected ()
  ;; Signals user-error when not connected.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () nil)))
    (should-error (task-conductor-project--eval-or-error "(foo)")
                  :type 'user-error)))

;;; Session lookup tests

(ert-deftest task-conductor-project-find-session-match ()
  ;; Returns the session plist when a session matches the task-id.
  (let* ((session (list :session-id "s1" :task-id 42 :state :running))
         (task-conductor-dev-env--cached-sessions (list session)))
    (should (equal session (task-conductor-project--find-session 42)))))

(ert-deftest task-conductor-project-find-session-no-match ()
  ;; Returns nil when no session matches the task-id.
  (let* ((session (list :session-id "s1" :task-id 99 :state :running))
         (task-conductor-dev-env--cached-sessions (list session)))
    (should-not (task-conductor-project--find-session 42))))

(ert-deftest task-conductor-project-find-session-multiple-picks-correct ()
  ;; Returns the session whose :task-id matches, ignoring others.
  (let* ((s1 (list :session-id "s1" :task-id 10 :state :idle))
         (s2 (list :session-id "s2" :task-id 42 :state :running))
         (s3 (list :session-id "s3" :task-id 99 :state :idle))
         (task-conductor-dev-env--cached-sessions (list s1 s2 s3)))
    (should (equal s2 (task-conductor-project--find-session 42)))))

(ert-deftest task-conductor-project-find-session-numeric-vs-string ()
  ;; Matches when task-id is numeric and session stores it as string.
  (let* ((session (list :session-id "s1" :task-id "42" :state :running))
         (task-conductor-dev-env--cached-sessions (list session)))
    (should (equal session (task-conductor-project--find-session 42))))
  ;; Also matches when task-id is string and session stores it as numeric.
  (let* ((session (list :session-id "s2" :task-id 42 :state :idle))
         (task-conductor-dev-env--cached-sessions (list session)))
    (should (equal session (task-conductor-project--find-session "42")))))

(ert-deftest task-conductor-project-find-session-nil-task-id ()
  ;; Returns nil when task-id is nil.
  (let ((task-conductor-dev-env--cached-sessions
         (list (list :session-id "s1" :task-id 1))))
    (should-not (task-conductor-project--find-session nil))))

(ert-deftest task-conductor-project-find-session-with-project-dir ()
  ;; Filters by project-dir when provided.
  (let* ((s1 (list :session-id "s1" :task-id 5 :project-dir "/a"))
         (s2 (list :session-id "s2" :task-id 5 :project-dir "/b"))
         (task-conductor-dev-env--cached-sessions (list s1 s2)))
    (should (equal s2 (task-conductor-project--find-session 5 "/b")))
    (should (equal s1 (task-conductor-project--find-session 5 "/a")))
    (should-not (task-conductor-project--find-session 5 "/c"))))

;;; CLI task fetching tests

(defun tc-test--make-task-hash (id title type status &optional parent-id category)
  "Create a hash table representing a task as parseedn would return it."
  (let ((h (make-hash-table :test #'equal)))
    (puthash :id id h)
    (puthash :title title h)
    (puthash :type type h)
    (puthash :status status h)
    (when category (puthash :category category h))
    (when parent-id (puthash :parent-id parent-id h))
    h))

(ert-deftest task-conductor-project-parse-tasks-edn-nil ()
  ;; Returns nil for nil input.
  (should-not (task-conductor-project--parse-tasks-edn nil)))

(ert-deftest task-conductor-project-parse-tasks-edn-empty ()
  ;; Returns nil for empty/whitespace input.
  (should-not (task-conductor-project--parse-tasks-edn ""))
  (should-not (task-conductor-project--parse-tasks-edn "  ")))

(defun tc-test--make-result-ht (task-hashes)
  "Create a {:tasks [...]} result hash-table as parseedn would return it."
  (let ((ht (make-hash-table :test #'equal)))
    (puthash :tasks (vconcat task-hashes) ht)
    ht))

(ert-deftest task-conductor-project-parse-tasks-edn-valid ()
  ;; Parses {:tasks [...]} map from parseedn into list of plists.
  (cl-letf (((symbol-function 'parseedn-read-str)
             (lambda (_str)
               (tc-test--make-result-ht
                (list (tc-test--make-task-hash 42 "Do thing" "task" "open"
                                               nil "simple"))))))
    (let ((result (task-conductor-project--parse-tasks-edn "{:tasks [...]}")))
      (should (= 1 (length result)))
      (should (= 42 (plist-get (car result) :id)))
      (should (equal "Do thing" (plist-get (car result) :title)))
      (should (equal "task" (plist-get (car result) :type)))
      (should (equal "open" (plist-get (car result) :status)))
      (should (equal "simple" (plist-get (car result) :category))))))

(ert-deftest task-conductor-project-parse-tasks-edn-parse-error ()
  ;; Returns nil when parseedn signals an error.
  (cl-letf (((symbol-function 'parseedn-read-str)
             (lambda (_str) (error "parse error"))))
    (should-not (task-conductor-project--parse-tasks-edn "bad edn"))))

(ert-deftest task-conductor-project-fetch-tasks-returns-root-only ()
  ;; Filters out tasks with a :parent-id, keeping only root tasks.
  (cl-letf (((symbol-function 'task-conductor-project--call-mcp-tasks-status)
             (lambda (_path status)
               (if (equal status "open") "open-edn" nil)))
            ((symbol-function 'task-conductor-project--parse-tasks-edn)
             (lambda (edn-str)
               (when edn-str
                 (list (list :id 1 :title "root" :type "task"
                             :status "open" :category "simple")
                       (list :id 2 :title "child" :type "task"
                             :status "open" :category "simple"
                             :parent-id 99))))))
    (let ((result (task-conductor-project--fetch-tasks "/proj")))
      (should (= 1 (length result)))
      (should (= 1 (plist-get (car result) :id)))
      (should (equal "root" (plist-get (car result) :title))))))

(ert-deftest task-conductor-project-fetch-tasks-deduplicates ()
  ;; De-duplicates tasks that appear in both open and in-progress results.
  (cl-letf (((symbol-function 'task-conductor-project--call-mcp-tasks-status)
             (lambda (_path _status) "edn"))
            ((symbol-function 'task-conductor-project--parse-tasks-edn)
             (lambda (_edn-str)
               (list (list :id 5 :title "dup" :type "task"
                           :status "open" :category "simple")))))
    (let ((result (task-conductor-project--fetch-tasks "/proj")))
      (should (= 1 (length result)))
      (should (= 5 (plist-get (car result) :id))))))

(ert-deftest task-conductor-project-fetch-tasks-sorted-by-id ()
  ;; Returns tasks sorted by :id ascending.
  (cl-letf (((symbol-function 'task-conductor-project--call-mcp-tasks-status)
             (lambda (_path status)
               (if (equal status "open") "open" "inprogress")))
            ((symbol-function 'task-conductor-project--parse-tasks-edn)
             (lambda (edn-str)
               (cond
                ((equal edn-str "open")
                 (list (list :id 30 :title "third" :type "task"
                             :status "open" :category "simple")
                       (list :id 10 :title "first" :type "task"
                             :status "open" :category "simple")))
                ((equal edn-str "inprogress")
                 (list (list :id 20 :title "second" :type "task"
                             :status "in-progress" :category "simple")))))))
    (let ((result (task-conductor-project--fetch-tasks "/proj")))
      (should (= 3 (length result)))
      (should (= 10 (plist-get (nth 0 result) :id)))
      (should (= 20 (plist-get (nth 1 result) :id)))
      (should (= 30 (plist-get (nth 2 result) :id))))))

(ert-deftest task-conductor-project-fetch-tasks-output-keys ()
  ;; Returned plists have only the expected keys (no :parent-id).
  (cl-letf (((symbol-function 'task-conductor-project--call-mcp-tasks-status)
             (lambda (_path _status) "edn"))
            ((symbol-function 'task-conductor-project--parse-tasks-edn)
             (lambda (_edn-str)
               (list (list :id 7 :title "t" :type "task"
                           :status "open" :category "simple")))))
    (let* ((result (task-conductor-project--fetch-tasks "/proj"))
           (task (car result)))
      (should (plist-get task :id))
      (should (plist-get task :title))
      (should (plist-get task :type))
      (should (plist-get task :status))
      (should-not (plist-get task :parent-id)))))

(ert-deftest task-conductor-project-fetch-tasks-cli-error ()
  ;; Returns (:error ...) when CLI call signals an error.
  (cl-letf (((symbol-function 'task-conductor-project--call-mcp-tasks-status)
             (lambda (_path _status) (error "No such file"))))
    (let ((result (task-conductor-project--fetch-tasks "/proj")))
      (should (eq :error (car result)))
      (should (stringp (cadr result))))))

(ert-deftest task-conductor-project-fetch-tasks-empty-results ()
  ;; Returns empty list when CLI returns nil/empty for both statuses.
  (cl-letf (((symbol-function 'task-conductor-project--call-mcp-tasks-status)
             (lambda (_path _status) nil))
            ((symbol-function 'task-conductor-project--parse-tasks-edn)
             (lambda (_edn-str) nil)))
    (let ((result (task-conductor-project--fetch-tasks "/proj")))
      (should (listp result))
      (should (null result)))))

;;; Task formatting tests

(ert-deftest task-conductor-project-task-type-icon ()
  ;; Each task type maps to its bracketed icon.
  (should (equal "[T]" (task-conductor-project--task-type-icon "task")))
  (should (equal "[B]" (task-conductor-project--task-type-icon "bug")))
  (should (equal "[F]" (task-conductor-project--task-type-icon "feature")))
  (should (equal "[S]" (task-conductor-project--task-type-icon "story")))
  (should (equal "[C]" (task-conductor-project--task-type-icon "chore")))
  (should (equal "[?]" (task-conductor-project--task-type-icon "unknown")))
  (should (equal "[?]" (task-conductor-project--task-type-icon nil))))

(ert-deftest task-conductor-project-task-status-icon ()
  ;; Each task status maps to its bracketed icon.
  (should (equal "[ ]" (task-conductor-project--task-status-icon "open")))
  (should (equal "[>]" (task-conductor-project--task-status-icon "in-progress")))
  (should (equal "[x]" (task-conductor-project--task-status-icon "done")))
  (should (equal "[x]" (task-conductor-project--task-status-icon "closed")))
  (should (equal "[!]" (task-conductor-project--task-status-icon "blocked")))
  (should (equal "[ ]" (task-conductor-project--task-status-icon nil))))

(ert-deftest task-conductor-project-format-task-entry ()
  ;; Formats a task plist into the expected display string with play icon.
  (let* ((task-conductor-dev-env--cached-sessions nil)
         (task (list :id 42 :title "Do thing" :type "task" :status "open"))
         (result (task-conductor-project--format-task-entry task)))
    (should (string-match-p "\\[T\\]\\[ \\] #42 Do thing" result))
    (should (string-match-p "▶" result)))
  (let* ((task-conductor-dev-env--cached-sessions nil)
         (task (list :id 7 :title "Fix bug" :type "bug" :status "in-progress"))
         (result (task-conductor-project--format-task-entry task)))
    (should (string-match-p "\\[B\\]\\[>\\] #7 Fix bug" result))
    (should (string-match-p "▶" result))))

(ert-deftest task-conductor-project-task-execution-icon ()
  ;; Maps each session state to its execution icon, nil for unknown.
  (should (equal "⏸" (task-conductor-project--task-execution-icon :idle)))
  (should (equal "🔄" (task-conductor-project--task-execution-icon :running)))
  (should (equal "🔔" (task-conductor-project--task-execution-icon :escalated)))
  (should (equal "🔀" (task-conductor-project--task-execution-icon :wait-pr-merge)))
  (should-not (task-conductor-project--task-execution-icon nil))
  (should-not (task-conductor-project--task-execution-icon :other)))

(ert-deftest task-conductor-project-format-task-entry-with-session ()
  ;; Appends execution icon when a matching session is found.
  (let* ((session (list :session-id "s1" :task-id 42 :state :running))
         (task-conductor-dev-env--cached-sessions (list session))
         (task (list :id 42 :title "Do thing" :type "task" :status "open"))
         (result (task-conductor-project--format-task-entry task)))
    (should (string-match-p "🔄" result))
    (should (string-match-p "#42 Do thing" result))))

(ert-deftest task-conductor-project-format-task-entry-session-text-property ()
  ;; Icon carries task-conductor-task-id text property for later clickability.
  (let* ((session (list :session-id "s1" :task-id 10 :state :idle))
         (task-conductor-dev-env--cached-sessions (list session))
         (task (list :id 10 :title "A task" :type "task" :status "open"))
         (result (task-conductor-project--format-task-entry task))
         (icon-pos (string-match "⏸" result)))
    (should icon-pos)
    (should (equal 10 (get-text-property icon-pos 'task-conductor-task-id result)))))

(ert-deftest task-conductor-project-format-task-entry-no-session ()
  ;; Play icon appended with correct text properties when no session matches.
  (let* ((task-conductor-dev-env--cached-sessions nil)
         (task (list :id 42 :title "Do thing" :type "task" :status "open"))
         (result (task-conductor-project--format-task-entry task)))
    (should (string-match-p "#42 Do thing" result))
    (should (string-match-p "▶" result))
    (let ((icon-pos (string-match "▶" result)))
      (should (equal 42 (get-text-property icon-pos 'task-conductor-task-id result)))
      (should (get-text-property icon-pos 'mouse-face result))
      (should (get-text-property icon-pos 'keymap result)))))

;;; insert-task-children tests (cache-only)

(ert-deftest task-conductor-project-insert-task-children-cache-hit ()
  ;; Inserts task sections when cache has data for the path.
  (with-project-buffer
    (let ((inhibit-read-only t))
      (puthash "/proj"
               (list (list :id 1 :title "First" :type "task" :status "open")
                     (list :id 2 :title "Second" :type "bug" :status "done"))
               task-conductor-project--task-cache)
      (magit-insert-section (task-conductor-project-root)
        (task-conductor-project--insert-task-children "/proj"))
      (let ((text (buffer-string)))
        (should (string-match-p "\\[T\\]\\[ \\] #1 First" text))
        (should (string-match-p "\\[B\\]\\[x\\] #2 Second" text))))))

(ert-deftest task-conductor-project-insert-task-children-cache-miss ()
  ;; Inserts nothing and does not call fetch-tasks when path is not in cache.
  (with-project-buffer
    (let ((inhibit-read-only t)
          (fetch-called nil))
      (cl-letf (((symbol-function 'task-conductor-project--fetch-tasks)
                 (lambda (_path) (setq fetch-called t) nil)))
        (magit-insert-section (task-conductor-project-root)
          (task-conductor-project--insert-task-children "/proj"))
        (should-not fetch-called)
        (should (string-empty-p (buffer-string)))))))

(ert-deftest task-conductor-project-insert-task-children-cached-error ()
  ;; Inserts warning-face text when cache holds an error value.
  (with-project-buffer
    (let ((inhibit-read-only t))
      (puthash "/proj" (list :error "CLI not found")
               task-conductor-project--task-cache)
      (magit-insert-section (task-conductor-project-root)
        (task-conductor-project--insert-task-children "/proj"))
      (let ((text (buffer-string)))
        (should (string-match-p "CLI not found" text))))))

(ert-deftest task-conductor-project-insert-task-children-nil-path ()
  ;; Does nothing when project-path is nil.
  (with-project-buffer
    (let ((inhibit-read-only t))
      (magit-insert-section (task-conductor-project-root)
        (task-conductor-project--insert-task-children nil))
      (should (string-empty-p (buffer-string))))))

;;; Render + cache tests

(ert-deftest task-conductor-project-render-sections-start-hidden ()
  ;; Project entry sections start collapsed (hidden) after render.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "p" :project/path "/p")))
    (goto-char (point-min))
    (magit-section-forward)
    (should (oref (magit-current-section) hidden))))

(ert-deftest task-conductor-project-render-no-fetch-without-cache ()
  ;; --render does not call --fetch-tasks; lazy loading is deferred to expand.
  (with-project-buffer
    (let ((fetch-called nil))
      (cl-letf (((symbol-function 'task-conductor-project--fetch-tasks)
                 (lambda (_path) (setq fetch-called t) nil)))
        (task-conductor-project--render
         (list (list :project/name "p" :project/path "/p")))
        (should-not fetch-called)))))

(ert-deftest task-conductor-project-render-with-cached-tasks ()
  ;; Task children appear in buffer text when cache is pre-populated.
  (with-project-buffer
    (puthash "/proj"
             (list (list :id 10 :title "A task" :type "task" :status "open"))
             task-conductor-project--task-cache)
    (task-conductor-project--render
     (list (list :project/name "proj" :project/path "/proj")))
    (let ((text (buffer-string)))
      (should (string-match-p "proj" text))
      (should (string-match-p "\\[T\\]\\[ \\] #10 A task" text)))))

(ert-deftest task-conductor-project-task-section-stores-value ()
  ;; Task sections store the task plist as their value.
  (with-project-buffer
    (let ((task-data (list :id 5 :title "My task" :type "task" :status "open")))
      (puthash "/p" (list task-data) task-conductor-project--task-cache)
      (task-conductor-project--render
       (list (list :project/name "p" :project/path "/p")))
      (goto-char (point-min))
      (magit-section-forward)
      (let ((entry (magit-current-section)))
        ;; Show the collapsed section so we can navigate into it
        (magit-section-show entry)
        (magit-section-forward)
        (let ((section (magit-current-section)))
          (should (eq (oref section type) 'task-conductor-project-task))
          (should (equal 5 (plist-get (oref section value) :id))))))))

;;; Expansion state tests

(ert-deftest task-conductor-project-expanded-paths-empty ()
  ;; Returns empty list when all sections are collapsed.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "p" :project/path "/proj")))
    (should (null (task-conductor-project--expanded-paths)))))

(ert-deftest task-conductor-project-expanded-paths-after-show ()
  ;; Expanding a section adds its path to the expanded list.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "p" :project/path "/proj")))
    (goto-char (point-min))
    (magit-section-forward)
    (magit-section-show (magit-current-section))
    (should (equal '("/proj") (task-conductor-project--expanded-paths)))))

(ert-deftest task-conductor-project-reexpand-paths-restores ()
  ;; reexpand-paths makes matching sections visible after render.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "p" :project/path "/proj")))
    (should (null (task-conductor-project--expanded-paths)))
    (task-conductor-project--reexpand-paths '("/proj"))
    (should (equal '("/proj") (task-conductor-project--expanded-paths)))))

(ert-deftest task-conductor-project-reexpand-paths-selective ()
  ;; reexpand-paths only shows sections matching the given paths.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "a" :project/path "/a")
           (list :project/name "b" :project/path "/b")))
    (task-conductor-project--reexpand-paths '("/b"))
    (let ((paths (task-conductor-project--expanded-paths)))
      (should (= 1 (length paths)))
      (should (member "/b" paths))
      (should-not (member "/a" paths)))))

;;; Lazy loading tests

(ert-deftest task-conductor-project-check-lazy-load-fetches-on-cache-miss ()
  ;; check-lazy-load fetches and caches tasks for an expanded section with no cache entry.
  (with-project-buffer
    (let* ((proj (list :project/name "p" :project/path "/p"))
           (task-conductor-dev-env--cached-projects (list proj))
           (fetch-called nil))
      (cl-letf (((symbol-function 'task-conductor-project--fetch-tasks)
                 (lambda (_path)
                   (setq fetch-called t)
                   (list (list :id 1 :title "T" :type "task" :status "open")))))
        (task-conductor-project--render (list proj))
        (goto-char (point-min))
        (magit-section-forward)
        (magit-section-show (magit-current-section))
        (task-conductor-project--check-lazy-load)
        (should fetch-called)
        (should (gethash "/p" task-conductor-project--task-cache))))))

(ert-deftest task-conductor-project-check-lazy-load-skips-cache-hit ()
  ;; check-lazy-load does not re-fetch when section already has children in cache.
  (with-project-buffer
    (let* ((proj (list :project/name "p" :project/path "/p"))
           (fetch-called nil))
      (puthash "/p"
               (list (list :id 1 :title "T" :type "task" :status "open"))
               task-conductor-project--task-cache)
      (cl-letf (((symbol-function 'task-conductor-project--fetch-tasks)
                 (lambda (_path) (setq fetch-called t) nil)))
        (task-conductor-project--render (list proj))
        (goto-char (point-min))
        (magit-section-forward)
        (magit-section-show (magit-current-section))
        (task-conductor-project--check-lazy-load)
        (should-not fetch-called)))))

;;; Refresh tests

(ert-deftest task-conductor-project-refresh-queries-sessions-when-connected ()
  ;; Refresh calls query-sessions to update session cache before re-rendering.
  (with-project-buffer
    (let ((sessions-queried nil))
      (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
                 (lambda () t))
                ((symbol-function 'task-conductor-dev-env-query-sessions)
                 (lambda () (setq sessions-queried t)))
                ((symbol-function 'task-conductor-project--list)
                 (lambda () (list :status :ok :projects nil))))
        (task-conductor-project-refresh)
        (should sessions-queried)))))

(ert-deftest task-conductor-project-refresh-skips-query-sessions-when-not-connected ()
  ;; Refresh does not call query-sessions when not connected.
  (with-project-buffer
    (let ((sessions-queried nil))
      (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
                 (lambda () nil))
                ((symbol-function 'task-conductor-dev-env-query-sessions)
                 (lambda () (setq sessions-queried t))))
        (task-conductor-project-refresh)
        (should-not sessions-queried)))))

(ert-deftest task-conductor-project-refresh-clears-cache ()
  ;; Refresh clears the task cache so tasks are re-fetched lazily.
  (with-project-buffer
    (puthash "/p" '() task-conductor-project--task-cache)
    (should (= 1 (hash-table-count task-conductor-project--task-cache)))
    (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
               (lambda () nil)))
      (task-conductor-project-refresh))
    (should (= 0 (hash-table-count task-conductor-project--task-cache)))))

(ert-deftest task-conductor-project-refresh-prefetches-expanded ()
  ;; Refresh pre-fetches tasks for previously-expanded projects so they remain visible.
  (with-project-buffer
    (let* ((proj (list :project/name "p" :project/path "/p"))
           (fetch-called-for nil))
      (puthash "/p"
               (list (list :id 1 :title "T" :type "task" :status "open"))
               task-conductor-project--task-cache)
      (task-conductor-project--render (list proj))
      (goto-char (point-min))
      (magit-section-forward)
      (magit-section-show (magit-current-section))
      (cl-letf (((symbol-function 'task-conductor-project--fetch-tasks)
                 (lambda (path) (push path fetch-called-for) nil))
                ((symbol-function 'task-conductor-dev-env--connected-p)
                 (lambda () nil)))
        (task-conductor-project-refresh)
        (should (member "/p" fetch-called-for))))))

(ert-deftest task-conductor-project-refresh-preserves-expansion ()
  ;; After refresh, previously-expanded sections are re-expanded.
  (with-project-buffer
    (let* ((proj (list :project/name "p" :project/path "/p"))
           (tasks (list (list :id 1 :title "T" :type "task" :status "open")))
           (task-conductor-dev-env--cached-projects (list proj)))
      (puthash "/p" tasks task-conductor-project--task-cache)
      (task-conductor-project--render (list proj))
      (goto-char (point-min))
      (magit-section-forward)
      (magit-section-show (magit-current-section))
      (cl-letf (((symbol-function 'task-conductor-project--fetch-tasks)
                 (lambda (_path) tasks))
                ((symbol-function 'task-conductor-dev-env--connected-p)
                 (lambda () t))
                ((symbol-function 'task-conductor-dev-env-query-sessions)
                 (lambda () nil))
                ((symbol-function 'task-conductor-project--list)
                 (lambda () (list :status :ok :projects (list proj)))))
        (task-conductor-project-refresh)
        (should (member "/p" (task-conductor-project--expanded-paths)))))))

(ert-deftest task-conductor-project-rerender-preserves-expansion ()
  ;; rerender-if-live preserves expanded sections across re-renders.
  (let* ((buf-name "*tc-test-rerender-expand*")
         (task-conductor-project--buffer-name buf-name)
         (task-conductor-dev-env--cached-projects
          (list (list :project/name "p" :project/path "/re")))
         (buf (get-buffer-create buf-name)))
    (unwind-protect
        (with-current-buffer buf
          (task-conductor-project-mode)
          (task-conductor-project--render
           task-conductor-dev-env--cached-projects)
          (goto-char (point-min))
          (magit-section-forward)
          (magit-section-show (magit-current-section))
          (should (equal '("/re") (task-conductor-project--expanded-paths)))
          (task-conductor-project-rerender-if-live)
          (should (equal '("/re") (task-conductor-project--expanded-paths))))
      (kill-buffer buf))))

(ert-deftest task-conductor-project-t-key-not-bound ()
  ;; t should not be bound in task-conductor-project-mode-map after
  ;; removing the mcp-tasks-browser keybinding.
  (with-project-buffer
    (should-not (lookup-key task-conductor-project-mode-map (kbd "t")))))

;;; Execute command tests

(defun tc-test--render-with-task (task-id project-path)
  "Set up a project buffer with one task under PROJECT-PATH and navigate to it.
Returns the project buffer, leaving point on the task section."
  (let* ((task-data (list :id task-id :title "Do thing" :type "task" :status "open"))
         (proj (list :project/name "p" :project/path project-path))
         (task-conductor-dev-env--cached-sessions nil))
    (task-conductor-project-mode)
    (puthash project-path (list task-data) task-conductor-project--task-cache)
    (task-conductor-project--render (list proj))
    (goto-char (point-min))
    (magit-section-forward)
    (magit-section-show (magit-current-section))
    (magit-section-forward)))

(ert-deftest task-conductor-project-task-context-at-point-returns-context ()
  ;; Returns task-id and project-dir from task section.
  (with-project-buffer
    (tc-test--render-with-task 42 "/myproj")
    (let ((ctx (task-conductor-project--task-context-at-point)))
      (should ctx)
      (should (equal 42 (plist-get ctx :task-id)))
      (should (equal "/myproj" (plist-get ctx :project-dir))))))

(ert-deftest task-conductor-project-task-context-at-point-not-on-task ()
  ;; Returns nil when point is on a project entry section, not a task.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "p" :project/path "/p")))
    (goto-char (point-min))
    (magit-section-forward)
    (should-not (task-conductor-project--task-context-at-point))))

(ert-deftest task-conductor-project-execute-no-task-at-point ()
  ;; Signals user-error when not on a task section.
  (with-project-buffer
    (task-conductor-project--render
     (list (list :project/name "p" :project/path "/p")))
    (goto-char (point-min))
    (should-error (task-conductor-project-execute) :type 'user-error)))

(ert-deftest task-conductor-project-execute-not-connected ()
  ;; Signals user-error when not connected.
  (with-project-buffer
    (tc-test--render-with-task 5 "/proj")
    (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
               (lambda () nil)))
      (should-error (task-conductor-project-execute) :type 'user-error))))

(ert-deftest task-conductor-project-execute-success ()
  ;; Calls eval-sync with execute! form and shows confirmation message.
  (with-project-buffer
    (tc-test--render-with-task 7 "/proj")
    (let ((eval-form nil))
      (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
                 (lambda () t))
                ((symbol-function 'task-conductor-dev-env--eval-sync)
                 (lambda (form)
                   (setq eval-form form)
                   (list :status :ok :session-id "s1"))))
        (task-conductor-project-execute)
        (should eval-form)
        (should (string-match-p "execute-task-by-id" eval-form))
        (should (string-match-p "/proj" eval-form))
        (should (string-match-p "7" eval-form))))))

(ert-deftest task-conductor-project-execute-error-result ()
  ;; Shows error message when execute-task-by-id returns :error status.
  (with-project-buffer
    (tc-test--render-with-task 3 "/proj")
    (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
               (lambda () t))
              ((symbol-function 'task-conductor-dev-env--eval-sync)
               (lambda (_form)
                 (list :status :error :message "task not found"))))
      ;; Should not signal; just display error message
      (task-conductor-project-execute))))

(ert-deftest task-conductor-project-execute-nrepl-error ()
  ;; Shows error message when nREPL signals an error (not just returns one).
  (with-project-buffer
    (tc-test--render-with-task 4 "/proj")
    (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
               (lambda () t))
              ((symbol-function 'task-conductor-dev-env--eval-sync)
               (lambda (_form)
                 (error "nREPL connection lost"))))
      ;; Should not signal; eval-or-error catches and returns error plist
      (task-conductor-project-execute))))

(ert-deftest task-conductor-project-e-key-bound ()
  ;; e is bound to task-conductor-project-execute in the mode map.
  (with-project-buffer
    (should (eq #'task-conductor-project-execute
                (lookup-key task-conductor-project-mode-map (kbd "e"))))))

;;; Cancel command tests

(defun tc-test--render-with-session (task-id project-path)
  "Set up project buffer with a task and leave point on the task section.
The caller must ensure a dynamic binding for
`task-conductor-dev-env--cached-sessions' is in effect before calling."
  (let* ((task-data (list :id task-id :title "Running" :type "task"
                          :status "in-progress"))
         (proj (list :project/name "p" :project/path project-path)))
    (task-conductor-project-mode)
    (puthash project-path (list task-data) task-conductor-project--task-cache)
    (task-conductor-project--render (list proj))
    (goto-char (point-min))
    (magit-section-forward)
    (magit-section-show (magit-current-section))
    (magit-section-forward)))

(ert-deftest task-conductor-project-cancel-no-task-at-point ()
  ;; cancel raises user-error when point is on a project entry, not a task.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t)))
    (with-project-buffer
      (task-conductor-project--render
       (list (list :project/name "p" :project/path "/p")))
      (goto-char (point-min))
      (magit-section-forward)
      (should-error (task-conductor-project-cancel) :type 'user-error))))

(ert-deftest task-conductor-project-cancel-not-connected ()
  ;; cancel raises user-error via eval-or-error when not connected.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () nil)))
    (with-project-buffer
      (let ((task-conductor-dev-env--cached-sessions
             (list (list :session-id "s1" :task-id 5
                         :state :running :project-dir "/proj"))))
        (tc-test--render-with-session 5 "/proj" "s1" :running)
        (should-error (task-conductor-project-cancel) :type 'user-error)))))

(ert-deftest task-conductor-project-cancel-no-session ()
  ;; cancel raises user-error when no active session exists for the task.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t)))
    (with-project-buffer
      (let ((task-conductor-dev-env--cached-sessions nil))
        (tc-test--render-with-task 42 "/proj")
        (should-error (task-conductor-project-cancel) :type 'user-error)))))

(ert-deftest task-conductor-project-cancel-not-running ()
  ;; cancel raises user-error when session state is not running or escalated.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t)))
    (with-project-buffer
      (let ((task-conductor-dev-env--cached-sessions
             (list (list :session-id "s1" :task-id 42
                         :state :idle :project-dir "/proj"))))
        (tc-test--render-with-session 42 "/proj" "s1" :idle)
        (should-error (task-conductor-project-cancel) :type 'user-error)))))

(ert-deftest task-conductor-project-cancel-calls-stop-when-running ()
  ;; cancel calls stop! with the correct session-id for a running task.
  (let ((eval-form nil))
    (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
               (lambda () t))
              ((symbol-function 'task-conductor-project--eval-or-error)
               (lambda (form) (setq eval-form form) "sid1")))
      (with-project-buffer
        (let ((task-conductor-dev-env--cached-sessions
               (list (list :session-id "sid1" :task-id 7
                           :state :running :project-dir "/proj"))))
          (tc-test--render-with-session 7 "/proj" "sid1" :running)
          (task-conductor-project-cancel)
          (should eval-form)
          (should (string-match-p "stop!" eval-form))
          (should (string-match-p "sid1" eval-form)))))))

(ert-deftest task-conductor-project-cancel-calls-stop-when-escalated ()
  ;; cancel calls stop! with the correct session-id for an escalated task.
  (let ((eval-form nil))
    (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
               (lambda () t))
              ((symbol-function 'task-conductor-project--eval-or-error)
               (lambda (form) (setq eval-form form) "sid-esc")))
      (with-project-buffer
        (let ((task-conductor-dev-env--cached-sessions
               (list (list :session-id "sid-esc" :task-id 9
                           :state :escalated :project-dir "/ep"))))
          (tc-test--render-with-session 9 "/ep" "sid-esc" :escalated)
          (task-conductor-project-cancel)
          (should (string-match-p "stop!" eval-form))
          (should (string-match-p "sid-esc" eval-form)))))))

(ert-deftest task-conductor-project-cancel-nrepl-error ()
  ;; cancel raises user-error when nREPL returns an error.
  (cl-letf (((symbol-function 'task-conductor-dev-env--connected-p)
             (lambda () t))
            ((symbol-function 'task-conductor-project--eval-or-error)
             (lambda (_form) (list :status :error :message "not found"))))
    (with-project-buffer
      (let ((task-conductor-dev-env--cached-sessions
             (list (list :session-id "s3" :task-id 3
                         :state :running :project-dir "/p3"))))
        (tc-test--render-with-session 3 "/p3" "s3" :running)
        (should-error (task-conductor-project-cancel) :type 'user-error)))))

(ert-deftest task-conductor-project-k-key-bound ()
  ;; k is bound to task-conductor-project-cancel in the mode map.
  (with-project-buffer
    (should (eq #'task-conductor-project-cancel
                (lookup-key task-conductor-project-mode-map (kbd "k"))))))

;;; Stop icon tests

(ert-deftest task-conductor-project-format-task-entry-running-shows-stop-icon ()
  ;; Running session shows status icon and clickable stop icon.
  (let* ((session (list :session-id "s1" :task-id 42 :state :running))
         (task-conductor-dev-env--cached-sessions (list session))
         (task (list :id 42 :title "Do thing" :type "task" :status "in-progress"))
         (result (task-conductor-project--format-task-entry task)))
    (should (string-match-p "🔄" result))
    (should (string-match-p "⏹" result))
    (let ((stop-pos (string-match "⏹" result)))
      (should (equal 42 (get-text-property stop-pos 'task-conductor-task-id result)))
      (should (get-text-property stop-pos 'mouse-face result))
      (should (get-text-property stop-pos 'keymap result)))))

(ert-deftest task-conductor-project-format-task-entry-escalated-shows-stop-icon ()
  ;; Escalated session shows status icon and clickable stop icon.
  (let* ((session (list :session-id "s2" :task-id 10 :state :escalated))
         (task-conductor-dev-env--cached-sessions (list session))
         (task (list :id 10 :title "A task" :type "task" :status "in-progress"))
         (result (task-conductor-project--format-task-entry task)))
    (should (string-match-p "🔔" result))
    (should (string-match-p "⏹" result))))

(ert-deftest task-conductor-project-format-task-entry-idle-no-stop-icon ()
  ;; Idle session shows status icon but no stop icon.
  (let* ((session (list :session-id "s3" :task-id 5 :state :idle))
         (task-conductor-dev-env--cached-sessions (list session))
         (task (list :id 5 :title "Idle task" :type "task" :status "open"))
         (result (task-conductor-project--format-task-entry task)))
    (should (string-match-p "⏸" result))
    (should-not (string-match-p "⏹" result))))

(provide 'task-conductor-project-test)
;;; task-conductor-project-test.el ends here
