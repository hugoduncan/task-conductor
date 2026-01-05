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
