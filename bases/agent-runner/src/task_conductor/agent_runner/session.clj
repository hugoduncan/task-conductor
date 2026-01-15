(ns task-conductor.agent-runner.session
  "Session persistence demonstration for Claude Agent SDK.

   This namespace demonstrates session lifecycle management and hand-off
   patterns between the Claude Agent SDK and Claude Code CLI. Sessions
   are persisted by the Claude infrastructure, enabling:

   1. SDK session persistence - pause and resume within SDK
   2. SDK→CLI hand-off - start in SDK, continue in CLI
   3. CLI→SDK hand-off - start in CLI, continue in SDK

   The session-id is the key identifier that enables hand-off between
   different clients (SDK or CLI)."
  (:require
   [task-conductor.agent-runner.cli :as cli]
   [task-conductor.claude-agent-sdk.interface :as sdk]))

;;; Session Lifecycle Functions

(defn- build-cli-opts
  "Build CLI options map from session config/opts and prompt.
  Handles :cwd/:working-dir normalization and :timeout-ms."
  [opts prompt]
  (let [working-dir (or (:cwd opts)
                        (:working-dir opts)
                        (System/getProperty "user.dir"))
        timeout-ms (:timeout-ms opts)]
    (cond-> {:working-dir working-dir
             :prompt prompt}
      timeout-ms (assoc :timeout-ms timeout-ms))))

(defn run-and-capture-session
  "Run a prompt and capture the session-id for later resumption.

   Executes a single prompt in a new session via the CLI, ensuring
   the session has Claude Code's system prompt, CLAUDE.md context,
   and MCP tools.

   Options:
   - :cwd - working directory for Claude (default: current directory)
   - :timeout-ms - timeout for CLI operation (default: 120000ms)

   Returns a map with:
   - :session-id - the session identifier for resumption
   - :messages - empty vector (CLI doesn't return message history)
   - :result - the parsed JSON response from CLI

   Throws ex-info on failure with :type key:
   - :cli/timeout - process exceeded timeout
   - :cli/non-zero-exit - process exited with non-zero code
   - :cli/parse-error - failed to parse JSON output
   - :cli/missing-session-id - response lacks session_id field

   Example:
     (run-and-capture-session \"Remember: X=42\" {:cwd \"/project\"})
     ;; => {:session-id \"abc-123\" :messages [] :result {...}}"
  ([prompt]
   (run-and-capture-session prompt {}))
  ([prompt opts]
   (let [{:keys [session-id response]} (cli/create-session-via-cli
                                        (build-cli-opts opts prompt))]
     {:session-id session-id
      :messages []
      :result response})))

(defn run-cli-session
  "Run a CLI session with the given config and prompt.

   Creates a new session via CLI (ensuring Claude Code's system prompt,
   CLAUDE.md context, and MCP tools), sends the prompt, and returns the
   result with session-id.

   This is the CLI equivalent of sdk/with-session for new sessions.

   Config options:
   - :cwd - working directory for Claude (default: current directory)
   - :timeout-ms - timeout for CLI operation (default: 120000ms)

   Returns {:result {:messages [] :response <cli-response>} :session-id <string>}.

   Note: CLI sessions don't return message history, so :messages is empty."
  [session-config prompt]
  (let [{:keys [session-id response]} (cli/create-session-via-cli
                                       (build-cli-opts session-config prompt))]
    {:result {:messages []
              :response response}
     :session-id session-id}))

(defn resume-session
  "Resume a previous session and run a follow-up prompt.

   Reconnects to an existing session by session-id and sends
   a new prompt. The session history is preserved.

   Options map supports all claude-agent-sdk options plus:
   - :max-turns - limit turns (default: 1)

   Returns a map with:
   - :session-id - the session identifier (may be updated)
   - :messages - vector of response messages
   - :result - the last message result

   Example:
     (resume-session \"abc-123\" \"What was X?\" {})
     ;; => {:session-id \"abc-123\" :messages [...] :result {...}}"
  ([session-id prompt]
   (resume-session session-id prompt {}))
  ([session-id prompt opts]
   (let [default-opts {:max-turns 1
                       :permission-mode "bypassPermissions"}
         merged-opts (merge default-opts opts)
         client (sdk/resume-client session-id merged-opts)]
     (try
       (sdk/connect client)
       (let [result (sdk/query client prompt)]
         {:session-id (:session-id result)
          :messages (:messages result)
          :result result})
       (finally
         (sdk/disconnect client))))))

(comment
  ;;; Session Lifecycle
  ;;
  ;; The Claude Agent SDK manages sessions through a connect/query/disconnect
  ;; lifecycle. Each session has a unique identifier that persists across
  ;; client restarts.
  ;;
  ;; Session Lifecycle:
  ;;   1. create-client - instantiate SDK client with options
  ;;   2. connect - establish session (generates session-id)
  ;;   3. query - send prompts, receive responses (session-id in ResultMessage)
  ;;   4. disconnect - end session (session persists for resumption)
  ;;
  ;; The session-id enables:
  ;;   - Resuming later with the same SDK client
  ;;   - Handing off to Claude Code CLI
  ;;   - Handing off from CLI back to SDK

  ;; Initialize the SDK (required once per JVM)
  (sdk/initialize! {:venv-path ".venv"})

  ;;; Example 1: Basic Session with ID Capture
  ;;
  ;; Run a prompt and capture the session-id for later use.

  (def session-result
    (run-and-capture-session "Remember this: the secret code is 42"))

  (:session-id session-result)
  ;; => "abc-123-def-456"

  ;;; Example 2: Resume Session
  ;;
  ;; Use the captured session-id to continue the conversation.

  (def resumed-result
    (resume-session (:session-id session-result)
                    "What was the secret code?"))

  ;; The assistant should recall "42" from the previous turn.

  ;;; SDK→CLI Hand-off
  ;;
  ;; Start a session in SDK, then continue in Claude Code CLI.
  ;;
  ;; 1. Run prompt in SDK and capture session-id:
  ;;    (def result (run-and-capture-session "Remember: project=alpha"))
  ;;
  ;; 2. Use session-id with CLI:
  ;;    $ claude --resume <session-id> --print -p "What project?"
  ;;
  ;; The CLI will have access to the full conversation history.

  ;;; CLI→SDK Hand-off
  ;;
  ;; Start a session in Claude Code CLI, then continue in SDK.
  ;;
  ;; 1. Run claude CLI and capture session-id from output:
  ;;    $ claude --print -p "Remember: task=cleanup"
  ;;    (session-id appears in output or can be extracted from result)
  ;;
  ;; 2. Resume in SDK:
  ;;    (resume-session "<session-id-from-cli>" "What was the task?")
  ;;
  ;; This enables workflows where humans interact via CLI, then
  ;; hand off to automated SDK processing.

  ;;; Low-level API Examples
  ;;
  ;; For more control, use the claude-agent-sdk interface directly.

  ;; Manual session management with TrackedClient
  (let [client (sdk/create-client {:max-turns 1
                                   :permission-mode "bypassPermissions"})]
    (try
      (sdk/connect client)
      (let [tracked (sdk/make-tracked-client client)
            result (sdk/session-query tracked "Hello!")
            sid (sdk/get-session-id tracked)]
        (println "Session ID:" sid)
        result)
      (finally
        (sdk/disconnect client))))

  ;; Using with-session macro (automatic lifecycle)
  (sdk/with-session [client {:max-turns 1
                             :permission-mode "bypassPermissions"}]
    (sdk/session-query client "First message")
    (sdk/session-query client "Second message"))
  ;; => {:result {...} :session-id "xyz"}

  ;; Fork a session (branch without modifying original)
  (let [client (sdk/fork-client "original-session-id"
                                {:max-turns 1
                                 :permission-mode "bypassPermissions"})]
    (try
      (sdk/connect client)
      (sdk/query client "Continue from fork...")
      (finally
        (sdk/disconnect client))))

  ;;; Session ID Format
  ;;
  ;; Session IDs are opaque strings assigned by the Claude infrastructure.
  ;; They are consistent between SDK and CLI, enabling hand-off.
  ;; Format example: "01234567-89ab-cdef-0123-456789abcdef"
  )
