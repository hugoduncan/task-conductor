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

**Hook types**: `:on-close`, `:on-idle`

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

## Error Handling

Return error maps rather than throwing exceptions. Errors are data that statecharts can route, not control flow interruptions.

```clojure
{:error :error-keyword
 :message "Human readable message"}
```

## Git Hooks

Pre-commit hook runs cljfmt and clj-kondo on staged files. Configure with:
```bash
git config core.hooksPath .githooks
```
