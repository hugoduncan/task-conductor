# LEARNING.md

Past discoveries and learnings.

## 2026-02-22: Statechart Dynamic Transitions

### `apply sc/state` for Dynamic Transition Lists
- Use `(apply sc/state {:id :foo} fixed-transition1 fixed-transition2 (dynamic-transitions-vec))` to append a computed vector of transitions to a state definition
- Self-transitions on states with on-entry actions re-trigger the entry action — avoid them

## 2026-02-21: Emacs Dev-Env Setup Order

### Don't Pre-Register on JVM Before CIDER Connects
- `register-emacs-dev-env` on JVM before CIDER connect creates an orphan entry
- CIDER connect triggers Emacs to auto-register its own dev-env ID
- Result: JVM has one ID, Emacs has another — mismatch
- Correct order: (1) CIDER connect, (2) `task-conductor-dev-env-connect` from Emacs
- If orphans exist: disconnect Emacs, `unregister!` JVM entries, reconnect from Emacs

## 2026-02-20: Story Execution Debugging

### CLAUDECODE Env Var Blocks Subprocess
- Claude CLI refuses to start when `CLAUDECODE=1` is inherited from parent session
- Fix: `{:extra-env {"CLAUDECODE" ""}}` in babashka.process opts
- Symptom: exit code 1, zero events, nil session-id

### mcp-tasks Meta Keys Are Namespaced
- CLI returns meta as `#:user{:refined "true"}` — key is `:user/refined`, not `:refined`
- `refined?` must compare `(name k)` to handle any namespace

### Statechart Re-registration on Reload
- `(require 'task-conductor.project.execute :reload)` fails: "Chart already registered"
- Workaround: `(in-ns 'task-conductor.project.execute)` then redefine individual fns
- Root cause: `register-statecharts!` runs at namespace load time

### Emacs Hook Timing
- `register-hook` sent after `start-session` can fail if buffer dies before command arrives
- Fix: always add `kill-buffer-hook` during `start-session` itself
- `on-close-handler` should fire unconditionally for managed sessions

## 2026-02-20: Emacs Dev-Env Startup

### Multiple Emacs Server Sockets
- GUI Emacs may have its own server socket (e.g., `server3766`) separate from the default `server`
- Verify with `emacsclient -e '(emacs-pid)'` vs `M-: (emacs-pid)` in GUI
- Use `emacsclient -s server<PID>` to target the correct instance
- Server sockets found under `$TMPDIR/emacs<UID>/`

### parseedn Returns Vectors, Not Lists
- `parseedn-read-str` returns EDN vectors `[...]` as Emacs vectors, not lists
- `dolist`, `car`, `cdr` fail on vectors with `wrong-type-argument listp`
- Fix: coerce with `(append vec nil)` or add `vectorp` clause in recursive converters

### Bootstrap Loads All Resolvers
- `(task-conductor.agent-runner.core/bootstrap!)` loads all 5 resolver namespaces
- Don't require individual resolver namespaces — use bootstrap
- Must use heredoc for shell calls containing `!`

## 2026-02-02: EQL work-on! Debugging Session

### Parseedn UUID Handling
- `parseedn-read-str` returns UUIDs as `(edn-uuid "uuid-string")` not plain strings
- `edn-to-plist` conversion must handle this: extract the string from the list
- Fix: check for `(eq (car edn) 'edn-uuid)` and return `(cadr edn)`

### CLI Returns Strings, Not Keywords
- mcp-tasks CLI returns `:type` as `"story"` (string), not `:story` (keyword)
- Same for `:status`: `"open"`, `"closed"`, `"deleted"` are strings
- Resolvers and state derivation must handle both: `(or (= :story t) (= "story" t))`

### Optional Fields in Pathom Resolvers and Mutations
- If a resolver/mutation declares `::pco/output`, Pathom expects all keys present
- CLI may not return optional fields (`:pr-num`, `:code-reviewed`) when nil
- Fix for resolvers: merge nil defaults before prefixing keys:
  ```clojure
  (merge {:task/pr-num nil :task/code-reviewed nil :task/error nil}
         (prefix-keys (:task result)))
  ```
- Fix for mutations: always return all declared output keys, including `nil` for absent ones
- Without this, mutation joins fail: Pathom tries to resolve missing keys via non-existent resolvers

### EQL Mutation Calling Convention
- Mutations in EQL must be **quoted symbols**, not evaluated function calls
- `(graph/query [{(my-mutation! params) [:key]}])` — WRONG: evaluates mutation as function, result map becomes invalid join key
- `(graph/query [`{(my-mutation! ~params) [:key]}])` — correct: backtick keeps mutation as symbol list

### Deleted Tasks in State Derivation
- `list-tasks` with `:parent-id` filter returns deleted children
- `derive-story-state` was counting deleted tasks as incomplete
- Fix: filter out deleted children before checking completion:
  ```clojure
  (defn- active-children [children]
    (remove #(#{:deleted "deleted"} (:status %)) children))
  ```

### Skill Execution Model Mismatch
- `invoke-skill!` runs `/skill-name` via claude-cli and waits for completion
- Refinement skills need human interaction - they exit quickly without effect
- When skill completes but state unchanged, system gets stuck in same state
- Need: escalate to dev-env when re-derived state equals current state (task 377)

### Namespace Loading Order
- EQL resolvers auto-register on namespace load
- Must require all resolver namespaces before using dependent mutations
- `work-on!` needs: mcp-tasks/resolvers, dev-env/resolvers, emacs-dev-env/resolvers

### Multiple nREPL Servers
- Can have multiple nREPL servers on different ports (7888, 54158)
- CIDER connects to one; dev-env registry is per-JVM
- Ensure commands use correct port for the running system

## 2026-02-02: nREPL and Dev-Env Reset

### nREPL Server Restart
- Find process: `ps aux | grep nrepl` or check `.nrepl-port` file
- Kill and restart: `kill <pid> && clj -M:dev:nrepl -m nrepl.cmdline --port 7888`
- Verify: `clj-nrepl-eval --port 7888 "(+ 1 1)"`
- Exit code 143 = SIGTERM (expected when killing process)

### Interface Function Names
- pathom-graph: `reset-graph!` (not `reset!`)
- emacs-dev-env: `register-emacs-dev-env` takes no args for default
- emacs-dev-env: `shutdown` requires dev-env argument
- emacs-dev-env state clears on nREPL restart (in-memory atoms)

### Emacs Dev-Env Registration
- `(emacs-dev-env/register-emacs-dev-env)` - creates and registers default
- `(emacs-dev-env/list-dev-envs)` - verify registration, shows `{:connected? true}`

## 2026-02-08: Emacs Dev-Env Registration

### Registration Must Happen From Emacs
- `register-emacs-dev-env` called from JVM (clj-nrepl-eval) creates the dev-env but no Emacs poll loop starts
- `M-x task-conductor-dev-env-connect` must be used — it both registers AND starts the async poll timer
- Without the poll loop, commands (`:start-session`, etc.) sit on the channel and timeout after 30s
- `escalate-to-dev-env!` returns `{:escalate/status :escalated}` even on timeout — it doesn't check the response

## 2026-02-08: nREPL Shell Escaping and Alias Staleness

### Shell Escaping `!` in clj-nrepl-eval
- Symbols containing `!` get mangled by shell escaping in single-quoted `clj-nrepl-eval` args
- Use heredoc pattern to avoid escaping issues:
  ```bash
  read -r -d '' CODE << 'EoC' || true
  (let [f @(ns-resolve (the-ns 'task-conductor.dev-env.registry) 'unregister!)]
    (f "dev-env-34681"))
  EoC
  clj-nrepl-eval --port PORT "$CODE"
  ```

### Stale Namespace Aliases in nREPL
- `require :reload` reloads namespace code but does NOT update aliases in the user ns if the var didn't exist when the alias was first created
- Workaround: use `ns-resolve` + `the-ns` to get the var directly
- Fully qualified calls also cache at compile time, so they fail the same way

### Dev-Env Registry API
- `unregister!` (not `deregister!`) removes a dev-env by ID, returns true/false
- `clear!` removes all entries (testing only)
- `select-dev-env` returns first available entry
