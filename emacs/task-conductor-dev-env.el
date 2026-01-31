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

;;;###autoload
(defun task-conductor-dev-env-connect ()
  "Connect to task-conductor and register as a dev-env.
This starts the command subscription loop that receives commands
from the orchestrator and dispatches them to claude-code.el."
  (interactive)
  (message "task-conductor-dev-env: connect not yet implemented"))

(provide 'task-conductor-dev-env)
;;; task-conductor-dev-env.el ends here
