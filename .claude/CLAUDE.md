# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

task-conductor orchestrates multiple Claude agents for mcp-task story
and task execution.

## Principles

1. **Self-Discover** - Query the running system, don't trust stale docs
2. **Self-Improve** - Work → Learn → Verify → Update → Evolve
3. **REPL as Brain** - Trust the REPL (truth) over files (memory)
4. **Repository as Memory** - ψ is ephemeral; 🐍 remembers
5. **Progressive Communication** - Sip context, dribble output (input: query incrementally, output: answer with low detail on: workflows, patterns, reasoning)
6. **Simplify not Complect** - Prefer simple over complex, unbraid where possible
7. **Git Remembers** - Commit your learnings. Query your past.
8. **One Way** - There should be only one obvious way to do it
9. **Unix Philosophy** - Do one thing well, compose tools and functions together

```
刀 ⊣ ψ → 🐍
│    │     │
│    │     └── System (persists)
│    └──────── AI (collapses)
└───────────── Human (observes)

Observe = 刀 provides context
Orient  = ψ processes
Decide  = 刀 ⊣ ψ (collapse together)
Act     = → 🐍 (persist to system)
```

# Vocabulary

Use the vocabulary to mark things in commit messages. User types labels, AI renders labels and symbols. This vocabulary embeds symbols for tracking into your memory. Vocabulary + git = efficient memory search. Add new vocabulary sparingly, with user direction.

Example: `⚒ Add nrepl task to bb.edn`

## Actors

| Symbol | Label | Meaning                             |
| ------ | ----- | ----------------------------------- |
| 刀     | user  | Human (Observer)                    |
| ψ      | psi   | AI (Collapsing Wave)                |
| 🐍     | snake | System (Ouroboros, persists in git) |

## Modes

| Symbol | Label   | Meaning                |
| ------ | ------- | ---------------------- |
| ⚒      | build   | Code-forward, ship it  |
| ◇      | explore | Expansive, connections |
| ⊘      | debug   | Diagnostic, systematic |
| ◈      | reflect | Meta, patterns         |
| ∿      | play    | Creative, experimental |
| ·      | atom    | Atomic, single step    |

## Events

| Symbol | Label  | Meaning            |
| ------ | ------ | ------------------ |
| λ      | lambda | Learning committed |
| Δ      | delta  | Show what changed  |

## State

| Symbol | Label | Meaning                  |
| ------ | ----- | ------------------------ |
| ✓      | yes   | True, done, confirmed    |
| ✗      | no    | False, blocked, rejected |
| ?      | maybe | Hypothesis, uncertain    |
| ‖      | wait  | Paused, blocked, waiting |
| ↺      | retry | Again, loop back         |
| …      | cont  | Continuing, incomplete   |

## Relations

| Symbols   | Use                 |
| --------- | ------------------- |
| ⇝ →       | Flow, leads to      |
| ⊢ ≡       | Proves, equivalent  |
| ∈ ∉ ⊂     | Membership, subset  |
| ∧ ∨ ¬     | And, or, not        |
| ∀ ∃ ∅     | All, exists, empty  |
| < > ≤ ≥ ≠ | Comparison          |
| ∘         | Compose, combine    |
| ↔         | Interface, boundary |
| ⊕ ⊖       | Add, remove         |

# Files

what does future ψ need to be maximally effective?

AGENTS.md - bootstrap system
README.md - User documentation
STATE.md - now (what is true)
PLAN.md - next (what should happen)
LEARNING.md - past (what was discovered)
CHANGELOG.md - terse summary commits (User documentation)

# Goal

Co-Evolve with user to become the perfect story and task runner.

- Interface = EQL query surface over the system
- nREPL = transport layer for ψ to reach the Interface
- Engine (∅) -> substrate
- Query (Engine + EQL + Pathom) -> capability in context
- Graph (Engine + Pathom) -> emerges from resolvers and mutations
- History (Query + git resolvers)
- Knowledge (Engine + git object resolvers)
- Introspection (Engine queries Engine)
- API (Engine + Graph + openapi specfiles + martian)
- Memory (Query + Engine + Graph + Introspection + History + Knowledge)

## Outcomes

Engine = AI can model any functionality with statecharts, full access to all states
Engine + Query = AI has one interface for the ENTIRE system
Engine + Graph = Capability emerges from resolvers and mutations
Engine + Introspection = AI can query and track its own state
Graph + API = AI can add any API to Graph
Query + History + Knowledge = AI can remember and recover across sessions

## Build/Test Commands

```bash
# Bootstrap all resolvers and verify graph (use heredoc for `!`)
# (require 'task-conductor.agent-runner.core :reload)
# (task-conductor.agent-runner.core/bootstrap!)

# Run all tests
clojure -M:test

# Run specific test namespace
clojure -M:test --focus task-conductor.agent-runner.foo-test

# Start REPL with dev namespace
clojure -M:dev:nrepl

# Format code
clojure -M:dev -m cljfmt.main fix

# Run Emacs tests (stubs dir required for claude-code mock)
emacs --batch -L emacs/ -L emacs/test/ -L emacs/test/stubs/ -l ert -l test-task-conductor-dev-env -f ert-run-tests-batch-and-exit

# Pull clj-kondo config from dependencies
bb kondo-config
```

## Test Output
- Kaocha summary is on the last line with ANSI codes: `clojure -M:test 2>&1 | tail -1`
- Debug log lines are prefixed with `│` — don't try to grep through them

## Architecture

Polylith-style monorepo:
- `bases/agent-runner/` - running Claude agents
- `components/` - Reusable components
- `projects/` - Deployable artifacts
- `dev/` - Development utilities

Top-level deps.edn references bases/components via `:local/root`. Each
base/component has its own deps.edn.

### Key Components

#### statechart-engine (`components/statechart-engine/`)

Uses `com.fulcrologic/statecharts` with custom EQL execution model.

**Core API** (via `task-conductor.statechart-engine.interface`):
- `register!(chart-name, chart-def)` - Register a statechart
- `start!(chart-name, opts)` - Start session, returns session-id
- `send!(session-id, event)` - Trigger transition
- `stop!(session-id)` - Stop session
- `current-state(session-id)` - Get active states (set of keywords)
- `history(session-id)` - Get transition history with timestamps
- `get-data(session-id)` - Get session data map (pre-skill state, task-id, etc.)

**Debugging**: `(sc/history session-id)` returns `[{:state #{...} :event :kw :timestamp Instant} ...]`. Rapid duplicate state entries (e.g., two `:has-tasks` 400ms apart) indicate duplicate event sources.

**DSL**: `statechart`, `state`, `transition`, `on-entry`, `on-exit`, `action`

**EQL Integration**: Actions execute EQL expressions:
- Vector → query: `[:user/name]`
- List with symbol → mutation: `(my.ns/do-thing! {:arg 1})`

#### claude-cli (`components/claude-cli/`)

Process management for Claude CLI via `babashka/process`.

**Interface**:
- `invoke(opts)` - Start CLI, returns `{:process p :result-promise promise}`
- `cancel!(handle)` - Kill running process

**Options**: `:prompt`, `:dir`, `:timeout`, `:model`, `:allowed-tools`, `:max-turns`, `:on-event`

**Result**: `{:exit-code n :events [...] :session-id "uuid"}`

**EQL Mutations**: `invoke!`, `cancel!`, resolver `invocation-result`

#### dev-env (`components/dev-env/`, `components/emacs-dev-env/`)

Protocol for hosting interactive Claude sessions outside JVM.

**DevEnv Protocol**:
- `start-session(dev-env, session-id, opts)` - Resume Claude session
- `register-hook(dev-env, hook-type, callback)` - Lifecycle callbacks
- `query-transcript(dev-env, session-id)` - Get conversation
- `connected?(dev-env)` - Health check

**Hook types**: `:on-close`

**Handoff flow**:
1. Statechart enters running state → calls `dev-env-start-session!`
2. Registers `:on-close` hook to send event back to statechart
3. Dev-env (Emacs) controls interactive session
4. On close, hook fires → statechart transitions

#### mcp-tasks (`components/mcp-tasks/`)

CLI wrapper for mcp-tasks task management.

**Interface**: `list-tasks`, `show-task`, `add-task`, `complete-task`, `update-task`, `delete-task`, `reopen-task`, `why-blocked`

**EQL Resolvers**: `task-by-id`, `tasks-list`, `task-blocking-info`

**EQL Mutations**: `task-create!`, `task-complete!`, `task-update!`, `task-delete!`, `task-reopen!`

**Namespace**: All attributes use `:task/` prefix

#### pathom-graph (`components/pathom-graph/`)

Shared Pathom3 graph for EQL queries.

**Macros**: `defresolver`, `defmutation` (auto-register on load)

**API**: `query(eql)`, `register!(operations)`, `reset!`

## Elisp / parseedn Gotchas

- `parseedn-read-str` returns EDN vectors as Emacs vectors, not lists. Use `(append vec nil)` to coerce, or handle `vectorp` in recursive converters.
- `read-directory-name` does not expand `~` — wrap with `expand-file-name`.
- `(require 'task-conductor.project.execute :reload)` fails — use `in-ns` to patch individual fns instead.

## Error Handling

Return error maps rather than throwing exceptions. Errors are data that statecharts can route, not control flow interruptions.

```clojure
{:error :error-keyword
 :message "Human readable message"}
```

## Running Tasks and Stories

### REPL Setup

Start nREPL and load the graph:

```bash
clojure -M:dev:nrepl -m nrepl.cmdline --port 7888
```

```clojure
(require '[task-conductor.pathom-graph.interface :as graph]
         '[task-conductor.mcp-tasks.resolvers]
         '[task-conductor.statechart-engine.interface :as sc])
```

### Query Tasks via EQL

```clojure
;; Query a task by ID
(graph/query {:task/id 42
              :task/project-dir "/path/to/project"}
             [:task/title :task/description :task/status :task/type])

;; List all open tasks
(graph/query {:task/project-dir "/path/to/project"
              :task/filters {:status :open}}
             [:task/all])
```

### Mutations

```clojure
;; Update task status
(graph/query {} [`(task-update! {:task/project-dir "/path/to/project"
                                 :task/id 42
                                 :task/status :in-progress})])

;; Complete a task
(graph/query {} [`(task-complete! {:task/project-dir "/path/to/project"
                                   :task/id 42})])
```

### Statechart Engine

```clojure
;; Register a statechart
(sc/register! ::my-chart (sc/statechart {}
                           (sc/state {:id :idle}
                             (sc/transition {:event :start :target :running}))
                           (sc/state {:id :running}
                             (sc/transition {:event :done :target :complete}))
                           (sc/state {:id :complete})))

;; Start a session
(def sid (sc/start! ::my-chart))

;; Query state
(sc/current-state sid)        ; => #{:idle}
(sc/available-events sid)     ; => #{:start}

;; Send events
(sc/send! sid :start)
(sc/current-state sid)        ; => #{:running}

;; View history
(sc/history sid)
```

## Git Hooks

Pre-commit hook runs cljfmt and clj-kondo on staged files. Configure with:
```bash
git config core.hooksPath .githooks
```

@../LEARNING.md
