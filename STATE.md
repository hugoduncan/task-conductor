# STATE.md

Current system state. Updated as part of story completion.

## Components

All 7 components have interfaces, implementations, and tests.

| Component | Status | Role |
|---|---|---|
| pathom-graph | ✓ | EQL query hub, singleton Pathom3 env |
| statechart-engine | ✓ | State machines with EQL action execution |
| mcp-tasks | ✓ | CLI wrapper for task management |
| claude-cli | ✓ | Process management for Claude CLI |
| dev-env | ✓ | Protocol for interactive session hosting |
| emacs-dev-env | ✓ | DevEnv impl via nREPL channels to Emacs |
| project | ✓ | Story/task orchestration via statecharts |

## Bases

| Base | Status | Role |
|---|---|---|
| agent-runner | ✗ stub | Entry point for autonomous execution |

## Integration

```
pathom-graph (hub)
  <- statechart-engine (actions execute EQL)
  <- project (execute!, escalate-to-dev-env!)
  <- mcp-tasks (task CRUD)
  <- dev-env / emacs-dev-env (interactive sessions)
  <- claude-cli (process spawning)
```

Execution flow: mcp-tasks story -> project/execute! -> statechart
drives state transitions -> skills via claude-cli -> escalation via
dev-env when stuck.

## Known Issues

- Emacs dev-env must be registered from Emacs (`M-x
  task-conductor-dev-env-connect`), not from JVM nREPL
- CLI returns string enums ("story", "open"); resolvers handle both
  strings and keywords
- `escalate-to-dev-env!` returns `:escalated` even on timeout
- Story 364 (line-breaker) merged but not closed in mcp-tasks
