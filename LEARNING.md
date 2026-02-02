# LEARNING.md

Past discoveries and learnings.

## 2026-02-02: EQL work-on! Debugging Session

### Parseedn UUID Handling
- `parseedn-read-str` returns UUIDs as `(edn-uuid "uuid-string")` not plain strings
- `edn-to-plist` conversion must handle this: extract the string from the list
- Fix: check for `(eq (car edn) 'edn-uuid)` and return `(cadr edn)`

### CLI Returns Strings, Not Keywords
- mcp-tasks CLI returns `:type` as `"story"` (string), not `:story` (keyword)
- Same for `:status`: `"open"`, `"closed"`, `"deleted"` are strings
- Resolvers and state derivation must handle both: `(or (= :story t) (= "story" t))`

### Optional Fields in Pathom Resolvers
- If a resolver declares outputs, Pathom expects them all to be present
- CLI may not return optional fields (`:pr-num`, `:code-reviewed`) when nil
- Fix: merge nil defaults before prefixing keys:
  ```clojure
  (merge {:task/pr-num nil :task/code-reviewed nil :task/error nil}
         (prefix-keys (:task result)))
  ```

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
