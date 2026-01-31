;;; task-conductor-dev-env.el --- Emacs dev-env for task-conductor -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: task-conductor contributors
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (cider "1.14.0") (claude-code "0.1.0"))
;; Keywords: tools, processes
;; URL: https://github.com/hugoduncan/task-conductor

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package registers Emacs as a dev-env with the task-conductor
;; orchestrator via nREPL.  It enables the orchestrator to delegate
;; interactive Claude sessions to Emacs, using claude-code.el as the
;; terminal backend.
;;
;; Usage:
;;   1. Start the task-conductor JVM with nREPL
;;   2. Connect to nREPL with M-x cider-connect
;;   3. Run M-x task-conductor-dev-env-connect
;;
;; The package polls the JVM for commands and dispatches them to
;; claude-code.el for execution.  Hook events (idle, close) are
;; sent back to the orchestrator.

;;; Code:

(require 'cider)

(defgroup task-conductor-dev-env nil
  "Emacs dev-env for task-conductor."
  :group 'tools
  :prefix "task-conductor-")

(defcustom task-conductor-nrepl-host "localhost"
  "Host for the task-conductor nREPL server."
  :type 'string
  :group 'task-conductor-dev-env)

(defcustom task-conductor-nrepl-port nil
  "Port for the task-conductor nREPL server.
When nil, uses the current CIDER connection."
  :type '(choice (const :tag "Use CIDER connection" nil)
                 (integer :tag "Port number"))
  :group 'task-conductor-dev-env)

;;; State

(defvar task-conductor-dev-env--dev-env-id nil
  "The dev-env-id returned from task-conductor registration.
This is a UUID string used to identify this Emacs instance.")

(defun task-conductor-dev-env--connected-p ()
  "Return non-nil if connected to task-conductor as a dev-env."
  (and task-conductor-dev-env--dev-env-id
       (cider-connected-p)))

;;; nREPL communication

(defun task-conductor-dev-env--eval-sync (form)
  "Evaluate Clojure FORM synchronously via nREPL.
Returns the value on success, or signals an error on failure."
  (unless (cider-connected-p)
    (user-error "Not connected to CIDER. Run M-x cider-connect first"))
  (let* ((result (cider-nrepl-sync-request:eval form))
         (value (nrepl-dict-get result "value"))
         (err (nrepl-dict-get result "err"))
         (ex (nrepl-dict-get result "ex")))
    (when (or err ex)
      (error "nREPL eval error: %s" (or err ex)))
    (when value
      (read value))))

;;; Connection management

;;;###autoload
(defun task-conductor-dev-env-connect ()
  "Connect to task-conductor and register as a dev-env.
Requires an active CIDER connection.  Stores the dev-env-id for
subsequent operations."
  (interactive)
  (when (task-conductor-dev-env--connected-p)
    (user-error "Already connected as dev-env %s" task-conductor-dev-env--dev-env-id))
  (let ((dev-env-id (task-conductor-dev-env--eval-sync
                     "(task-conductor.emacs-dev-env.interface/register-emacs-dev-env)")))
    (setq task-conductor-dev-env--dev-env-id dev-env-id)
    (message "Connected to task-conductor as dev-env: %s" dev-env-id)))

(defun task-conductor-dev-env-disconnect ()
  "Disconnect from task-conductor and unregister this dev-env."
  (interactive)
  (unless (task-conductor-dev-env--connected-p)
    (user-error "Not connected to task-conductor"))
  (let ((dev-env-id task-conductor-dev-env--dev-env-id))
    (task-conductor-dev-env--eval-sync
     (format "(task-conductor.emacs-dev-env.interface/unregister-emacs-dev-env %S)"
             dev-env-id))
    (setq task-conductor-dev-env--dev-env-id nil)
    (message "Disconnected from task-conductor")))

(provide 'task-conductor-dev-env)
;;; task-conductor-dev-env.el ends here
