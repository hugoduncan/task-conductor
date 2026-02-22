;;; task-conductor-nrepl.el --- Minimal nREPL client -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: task-conductor contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, processes

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Minimal nREPL client for task-conductor.  Speaks bencode over TCP
;; directly, avoiding any dependency on CIDER.

;;; Code:

(require 'cl-lib)

;;; Bencode codec

(defun task-conductor-nrepl--bencode-encode (obj)
  "Encode OBJ as a bencode string.
Supports strings, integers, lists (as bencode lists),
and alists with string keys (as bencode dicts)."
  (cond
   ((stringp obj)
    (format "%d:%s" (string-bytes obj) obj))
   ((integerp obj)
    (format "i%de" obj))
   ((and (listp obj)
         (consp (car obj))
         (stringp (caar obj)))
    ;; Alist with string keys → bencode dict
    (concat "d"
            (mapconcat (lambda (pair)
                         (concat (task-conductor-nrepl--bencode-encode (car pair))
                                 (task-conductor-nrepl--bencode-encode (cdr pair))))
                       obj "")
            "e"))
   ((listp obj)
    ;; List → bencode list
    (concat "l"
            (mapconcat #'task-conductor-nrepl--bencode-encode obj "")
            "e"))
   (t (error "Cannot bencode-encode: %S" obj))))

(defun task-conductor-nrepl--bencode-decode (str)
  "Decode bencode STR.
Return (VALUE . REST) where REST is the unconsumed portion of STR,
or nil if STR is incomplete."
  (when (and str (> (length str) 0))
    (let ((ch (aref str 0)))
      (cond
       ;; Integer: i<digits>e
       ((= ch ?i)
        (let ((end (cl-position ?e str)))
          (when end
            (cons (string-to-number (substring str 1 end))
                  (substring str (1+ end))))))
       ;; String: <len>:<data>
       ((and (>= ch ?0) (<= ch ?9))
        (let ((colon (cl-position ?: str)))
          (when colon
            (let* ((len (string-to-number (substring str 0 colon)))
                   (start (1+ colon))
                   (needed (+ start len)))
              (when (>= (length str) needed)
                (cons (substring str start needed)
                      (substring str needed)))))))
       ;; List: l...e
       ((= ch ?l)
        (let ((rest (substring str 1))
              (items nil))
          (catch 'incomplete
            (while (and rest (> (length rest) 0) (/= (aref rest 0) ?e))
              (let ((decoded (task-conductor-nrepl--bencode-decode rest)))
                (unless decoded (throw 'incomplete nil))
                (push (car decoded) items)
                (setq rest (cdr decoded))))
            (when (and rest (> (length rest) 0))
              (cons (nreverse items) (substring rest 1))))))
       ;; Dict: d...e
       ((= ch ?d)
        (let ((rest (substring str 1))
              (pairs nil))
          (catch 'incomplete
            (while (and rest (> (length rest) 0) (/= (aref rest 0) ?e))
              (let ((key-decoded (task-conductor-nrepl--bencode-decode rest)))
                (unless key-decoded (throw 'incomplete nil))
                (let ((val-decoded (task-conductor-nrepl--bencode-decode (cdr key-decoded))))
                  (unless val-decoded (throw 'incomplete nil))
                  (push (cons (car key-decoded) (car val-decoded)) pairs)
                  (setq rest (cdr val-decoded)))))
            (when (and rest (> (length rest) 0))
              (cons (nreverse pairs) (substring rest 1))))))
       (t nil)))))

;;; Connection object

(defun task-conductor-nrepl--next-id (conn)
  "Return the next message ID for CONN and increment counter."
  (let ((id (plist-get conn :counter)))
    (plist-put conn :counter (1+ id))
    (number-to-string id)))

(defun task-conductor-nrepl--send (conn msg)
  "Send bencode-encoded MSG alist over CONN."
  (let ((process (plist-get conn :process)))
    (when (and process (process-live-p process))
      (process-send-string process (task-conductor-nrepl--bencode-encode msg)))))

;;; Response accumulation

(defun task-conductor-nrepl--accumulate (conn id response)
  "Accumulate RESPONSE chunk for message ID on CONN.
Concatenates `out' and `err' values, keeps latest `value'.
Returns the accumulated response when status contains `done',
otherwise returns nil."
  (let* ((acc-table (plist-get conn :accumulator))
         (acc (or (gethash id acc-table)
                  (puthash id '() acc-table)))
         (out (task-conductor-nrepl-dict-get response "out"))
         (err (task-conductor-nrepl-dict-get response "err"))
         (value (task-conductor-nrepl-dict-get response "value"))
         (ex (task-conductor-nrepl-dict-get response "ex"))
         (status (task-conductor-nrepl-dict-get response "status")))
    (when out
      (let ((prev (or (task-conductor-nrepl-dict-get acc "out") "")))
        (setq acc (cons (cons "out" (concat prev out))
                        (cl-remove-if (lambda (p) (equal (car p) "out")) acc)))))
    (when err
      (let ((prev (or (task-conductor-nrepl-dict-get acc "err") "")))
        (setq acc (cons (cons "err" (concat prev err))
                        (cl-remove-if (lambda (p) (equal (car p) "err")) acc)))))
    (when value
      (setq acc (cons (cons "value" value)
                      (cl-remove-if (lambda (p) (equal (car p) "value")) acc))))
    (when ex
      (setq acc (cons (cons "ex" ex)
                      (cl-remove-if (lambda (p) (equal (car p) "ex")) acc))))
    (puthash id acc acc-table)
    (when (and status (member "done" status))
      (remhash id acc-table)
      ;; Merge status into accumulated response
      (cons (cons "status" status)
            (cl-remove-if (lambda (p) (equal (car p) "status")) acc)))))

;;; Process filter

(defun task-conductor-nrepl--filter (conn data)
  "Process filter: append DATA to CONN buffer, decode and dispatch."
  (plist-put conn :buffer (concat (plist-get conn :buffer) data))
  (let ((continue t))
    (while continue
      (let ((decoded (task-conductor-nrepl--bencode-decode (plist-get conn :buffer))))
        (if decoded
            (progn
              (plist-put conn :buffer (cdr decoded))
              (let* ((response (car decoded))
                     (id (task-conductor-nrepl-dict-get response "id"))
                     (final (when id
                              (task-conductor-nrepl--accumulate conn id response))))
                (when final
                  (let ((cb (gethash id (plist-get conn :pending))))
                    (when cb
                      (remhash id (plist-get conn :pending))
                      (funcall cb final))))))
          (setq continue nil))))))

;;; Public API

(defun task-conductor-nrepl-connect (host port)
  "Connect to nREPL server at HOST:PORT.
Returns a connection object or signals an error."
  (let* ((conn (list :process nil :session nil :buffer ""
                     :pending (make-hash-table :test 'equal)
                     :counter 0
                     :accumulator (make-hash-table :test 'equal)))
         (process (make-network-process
                   :name "task-conductor-nrepl"
                   :host host
                   :service port
                   :coding 'utf-8
                   :filter (lambda (_proc data)
                             (task-conductor-nrepl--filter conn data))
                   :sentinel (lambda (_proc event)
                               (unless (string-match-p "open" event)
                                 (message "task-conductor-nrepl: %s"
                                          (string-trim event)))))))
    (plist-put conn :process process)
    ;; Clone session
    (let ((session nil)
          (done nil)
          (clone-id (task-conductor-nrepl--next-id conn)))
      (puthash clone-id
               (lambda (response)
                 (setq session (task-conductor-nrepl-dict-get response "new-session"))
                 (setq done t))
               (plist-get conn :pending))
      (task-conductor-nrepl--send conn
                                  `(("op" . "clone") ("id" . ,clone-id)))
      ;; Wait for clone response (5s timeout)
      (let ((deadline (+ (float-time) 5.0)))
        (while (and (not done) (< (float-time) deadline))
          (accept-process-output process 0.1)))
      (unless session
        (delete-process process)
        (error "Failed to clone nREPL session"))
      (plist-put conn :session session)
      conn)))

(defun task-conductor-nrepl-connected-p (conn)
  "Return non-nil if CONN is live."
  (and conn
       (plist-get conn :process)
       (process-live-p (plist-get conn :process))))

(defun task-conductor-nrepl-close (conn)
  "Close nREPL connection CONN."
  (when conn
    (let ((process (plist-get conn :process)))
      (when (and process (process-live-p process))
        (delete-process process)))
    (plist-put conn :process nil)
    (plist-put conn :session nil)))

(defun task-conductor-nrepl-eval-sync (conn code &optional timeout)
  "Evaluate CODE synchronously on CONN.
Returns response alist.  TIMEOUT defaults to 30 seconds."
  (unless (task-conductor-nrepl-connected-p conn)
    (error "nREPL connection is not active"))
  (let* ((id (task-conductor-nrepl--next-id conn))
         (result nil)
         (done nil)
         (timeout-secs (or timeout 30)))
    (puthash id
             (lambda (response)
               (setq result response)
               (setq done t))
             (plist-get conn :pending))
    (task-conductor-nrepl--send conn
                                `(("op" . "eval")
                                  ("code" . ,code)
                                  ("session" . ,(plist-get conn :session))
                                  ("id" . ,id)))
    (let ((deadline (+ (float-time) timeout-secs)))
      (while (and (not done) (< (float-time) deadline))
        (accept-process-output (plist-get conn :process) 0.1)))
    (unless done
      (remhash id (plist-get conn :pending))
      (error "nREPL eval timed out after %ds" timeout-secs))
    result))

(defun task-conductor-nrepl-eval-async (conn code callback)
  "Evaluate CODE asynchronously on CONN.
CALLBACK is called with the response alist when done."
  (unless (task-conductor-nrepl-connected-p conn)
    (error "nREPL connection is not active"))
  (let ((id (task-conductor-nrepl--next-id conn)))
    (puthash id callback (plist-get conn :pending))
    (task-conductor-nrepl--send conn
                                `(("op" . "eval")
                                  ("code" . ,code)
                                  ("session" . ,(plist-get conn :session))
                                  ("id" . ,id)))))

(defun task-conductor-nrepl-dict-get (dict key)
  "Get KEY from response alist DICT."
  (cdr (assoc key dict)))

(provide 'task-conductor-nrepl)
;;; task-conductor-nrepl.el ends here
