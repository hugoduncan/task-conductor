# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

task-conductor orchestrates multiple Claude agents for complex task execution. Uses libpython-clj for Python/Claude SDK interop.

## Build/Test Commands

```bash
# Run all tests
clj -M:test

# Run specific test namespace
clj -M:test --focus task-conductor.agent-runner.foo-test

# Start REPL with dev namespace
clj -M:dev:nrepl

# Format code
clj -M:dev -m cljfmt.main fix

# Pull clj-kondo config from dependencies
bb kondo-config
```

## Architecture

Polylith-style monorepo:
- `bases/agent-runner/` - MCP server for running Claude agents (libpython-clj)
- `components/` - Reusable components (empty initially)
- `projects/` - Deployable artifacts (empty initially)
- `dev/` - Development utilities

Top-level deps.edn references bases/components via `:local/root`. Each base/component has its own deps.edn.

## Git Hooks

Pre-commit hook runs cljfmt and clj-kondo on staged files. Configure with:
```bash
git config core.hooksPath .githooks
```

## Working on Stories

### REPL Setup

```clojure
;; Required namespaces
(require '[task-conductor.claude-agent-sdk.interface :as sdk])
(require '[task-conductor.agent-runner.repl :as repl])
(require '[task-conductor.workspace.interface :as workspace])

;; Initialize Python SDK (once per JVM session)
(sdk/initialize! {:venv-path "components/claude-agent-sdk/.venv"})

;; Register project(s) and set focus
(workspace/add-project! "/path/to/project")
(workspace/set-focused-project! "/path/to/project")
```

### Executing Stories

```clojure
;; Run a story (non-blocking, returns immediately)
(repl/run-story 123)  ;; => {:status :started, :story-id 123}

;; Or specify workspace explicitly
(repl/run-story "/path/to/project" 123)
(repl/run-story :project-alias 123)  ;; keyword matching last path segment

;; Block until completion if needed
(repl/await-completion)  ;; => {:outcome :complete, :progress {...}, ...}
```

### Control During Execution

```clojure
(repl/status)      ;; Check state, :executing?, and :outcome
(repl/pause)       ;; Pause after current task
(repl/continue)    ;; Resume paused execution
(repl/abort)       ;; Cancel and return to idle
(repl/retry)       ;; Re-attempt failed task
(repl/skip)        ;; Skip failed task
```

### Context and Sessions

```clojure
(repl/add-context "Note for agents")  ;; Add to story shared-context
(repl/view-context)                    ;; Display shared-context
(repl/list-sessions)                   ;; List sessions for current story
```

### Reset State

```clojure
(require '[task-conductor.agent-runner.console :as console])
(console/reset-state!)           ;; Reset all workspaces
(console/reset-state! workspace) ;; Reset specific workspace
```


# nrepl

The command `clj-nrepl-eval` is installed on your path for evaluating Clojure code via nREPL.

**Discover nREPL servers:**

`clj-nrepl-eval --discover-ports`

Only ever use an nREPL server from your own worktree.

**Evaluate code:**

`clj-nrepl-eval -p <port> "<clojure-code>"`

With timeout (milliseconds)

`clj-nrepl-eval -p <port> --timeout 5000 "<clojure-code>"`

The REPL session persists between evaluations - namespaces and state are maintained.
Always use `:reload` when requiring namespaces to pick up changes.
