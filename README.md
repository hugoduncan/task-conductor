# task-conductor

Orchestrates multiple Claude agents for complex task execution.

## Setup

```bash
# Pull clj-kondo config from dependencies
bb kondo-config

# Start REPL with dev namespace
clj -M:dev:nrepl
```

## Development

```bash
# Run tests
clj -M:test

# Format code
clj -M:dev -m cljfmt.main fix
```
