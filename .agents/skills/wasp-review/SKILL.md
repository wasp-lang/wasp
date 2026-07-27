---
name: wasp-review
description: Review code changes for correctness, clarity, and potential issues. Use when the user asks for a review, code review, PR review, or wants feedback on a diff or file.
---

Review the specified PR, diff, or files. If none are specified, review the staged and unstaged changes.

## Naming And Vocabulary

- Demand precise, informative names.
- Names must not misdirect, omit relevant behavior, or assume unavailable context.
- Make names accurately reflect what their declarations represent.
- Avoid vague names such as `data`, `info`, or unexplained single letters.
- Review naming using Clean Code principles.
- Avoid variable shadowing.
- Context is kind: evaluate names within their scope and surrounding vocabulary.
- Use established codebase terminology consistently.
- Follow established naming conventions e.g. `is...` for booleans and `ensure...` for idempotent setup operations.
- Treat awkward names as evidence of design problems. If an accurate name becomes excessively long or complicated, inspect whether the declaration has too many responsibilities.

## Contracts And Interfaces

- Review names before implementation details. Poor names often expose deeper problems in decomposition and architecture.
- Declaration should be enough to understand, without the implementation. A function’s name, arguments, types, and contract should explain its behavior.

## Readability And Context

- Demand code to be easy to read. Understanding one section should not require reading the entire file.
- Review from the perspective of a developer with minimal reasonable context.

## Design And Architecture

- Treat `and` or `then` in a function name as a possible responsibility smell.
- Treat groups of similarly prefixed parameters as possible missing abstractions.
- Make sure the code is DRY. Introduce helpers for commonly repeated concepts.

## Assumptions And Correctness

- Identify hidden assumptions. Require assumptions to be enforced through types, runtime checks, or comments, in that order.
- Check for Effective TypeScript defined problems.

## Comments And Writing

- Prefer improving design or naming; use comments only when code cannot communicate enough.
- Use comments only for information that names, arguments, and types cannot express.
- Remove comments that merely narrate obvious code or exhibit generated filler.
- Suggest pruning fluff and no-op sentences from prose.

## Review Economics

- Check whether requested improvements justify their implementation cost.

## Language-Specific Tooling

- Run `shellcheck` when shell scripts change.

## Findings And Reporting

- Make every finding actionable. Include the file and line, explain the problem, and suggest a concrete fix.
