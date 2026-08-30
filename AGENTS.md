- Code and comments should be written in English.
- Solve root causes, not workarounds.
- When debugging a problem, do not guess and patch blindly. Add targeted debug logging (or other runtime evidence), identify the root cause, then implement the fix.
- Prefer simple implementations over complex ones.
- All observable behavior should match upstream DataScript.
- Implementation details should match upstream DataScript unless a divergence is explicitly requested and documented.
- Do not include user-specific absolute home-directory paths in code, comments, documentation, reports, tests, or committed examples. Use repository-relative paths, placeholders, or environment variables instead.

### OCaml codebase rules
- MUST not use magic methods to cast types. If a type cast is unavoidable, explain the reason in a code comment.
- MUST not disable compiler warnings

