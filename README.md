# task-conductor

Orchestrates multiple Claude agents for complex task execution.

## Setup

```bash
# Pull clj-kondo config from dependencies
bb kondo-config

# Start REPL with dev namespace
clj -M:dev:nrepl
```

## Running

### Python Environment

```bash
# Create virtual environment
python -m venv .venv

# Install Claude Agent SDK
.venv/bin/pip install -r components/claude-agent-sdk/requirements.txt
```

### Story Execution

From the REPL:

```clojure
;; Require the SDK and REPL namespaces
(require '[task-conductor.claude-agent-sdk.interface :as sdk])
(require '[task-conductor.agent-runner.repl :as repl])

;; Initialize the Python SDK (required once per JVM session)
(sdk/initialize! {:venv-path ".venv"})

;; Execute a story
(repl/run-story 123)
```

### Control Functions

```clojure
(repl/status)    ;; Check current state
(repl/pause)     ;; Pause after current task completes
(repl/continue)  ;; Resume paused execution
(repl/abort)     ;; Cancel and return to idle
```

## Development

```bash
# Run tests
clj -M:test

# Format code
clj -M:dev -m cljfmt.main fix
```
