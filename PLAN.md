# PLAN.md

Architectural direction and priorities. Updated as part of story completion.

## Vision

Engine that lets AI model any workflow as statecharts, query the
entire system via EQL, and remember across sessions via git.

```
Engine (statecharts) -> substrate
Query (Engine + EQL + Pathom) -> capability in context
Graph (Engine + Pathom) -> emerges from resolvers
Introspection (Engine queries Engine)
History (Query + git resolvers)
Knowledge (Engine + git object resolvers)
Memory (Query + Engine + Graph + Introspection + History + Knowledge)
```

## Current Priorities

1. **agent-runner base** (418) - Wire components into deployable
   orchestration. All pieces exist; nothing ties them together yet.
2. **Git History/Knowledge resolvers** (417) - Foundation for Memory.
   Without git resolvers, each session starts without queryable access
   to past work.
3. **CLI introspection resolvers** (376) - Make running Claude
   instances queryable via EQL for debugging and monitoring.

## Backlog

- Fix deprecated namespace in emacs-dev-env resolvers test (378)

## Architectural Decisions

- Error maps over exceptions — statecharts route errors as data
- Nullable testing pattern — components provide test doubles via
  `make-nullable` / `with-nullable-*` macros
- EQL as universal interface — all system interaction through Pathom
- Polylith structure — components compose via interfaces, bases deploy
