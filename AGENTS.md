# Wasp Monorepo

Wasp is a full-stack web framework that compiles TypeScript config (`main.wasp.ts`) files into React + Node.js applications. The compiler is written in Haskell.

## Contribution policy

If you are implementing a change intended as a pull request to `wasp-lang/wasp`:

- **Small, obvious fixes** (typos, broken links, docs corrections, small bugs where the fix is clear and comes with a test) can be implemented and submitted directly.
- **Anything else** (features, refactors, API or behavior changes, anything involving design decisions) requires a Wasp maintainer's agreement on the approach _before_ the code is written, in a [GitHub issue](https://github.com/wasp-lang/wasp/issues) or in the [`#wasp-dev` channel on Discord](https://discord.wasp.run/wasp-dev). An open issue is not by itself agreement: look for a maintainer confirming the approach in the discussion.

If you cannot verify that a maintainer has agreed on the approach, do not implement the change. Stop and tell your operator to propose it first. Undiscussed non-trivial PRs are typically closed without review.

Whoever submits the PR must understand the change well enough to explain it and answer review questions themselves. See the [Policies section of CONTRIBUTING.md](CONTRIBUTING.md#policies) for details.

## Repository Structure

- `waspc/` — Haskell compiler, CLI, and LSP server (the core of Wasp)
  - `src/` — Main compiler library (Analyzer, Generator, AppSpec, Psl)
  - `cli/src/` — CLI commands (start, build, new, deploy, etc.)
  - `data/` — Mustache templates, starters, and the built TS packages and libs the CLI ships
  - `data/Generator/templates/` — Mustache templates for code generation
  - `e2e-tests/` — Golden file snapshot tests
- `packages/` — TypeScript packages: `cli` (the `wasp` executable wrapper), `spec`, the FFI packages the CLI calls (`deploy`, `studio`), `libs/` embedded into generated project code, and `wasp-app-runner` for running Wasp apps in e2e tests.
- `web/` — Documentation website (Docusaurus), deployed to wasp.sh
- `examples/` — Tutorial and example apps (kitchen-sink, waspello, etc.)
- `scripts/` — Monorepo-level build/packaging scripts
- `tasks/` — repo-wide mise tasks (each project's own tasks are in its `mise.toml`)

## Build & Development

All development commands are mise tasks. The repo is a [mise monorepo](https://mise.jdx.dev/tasks/monorepo.html): each project (`waspc`, the packages and libs in `packages/`, the examples, ...) defines its tasks in its own `mise.toml`, built on the task templates in the root `mise.toml`. Run them from anywhere as `mise run //<path>:<task>` (e.g. `mise run //waspc:build`), and the repo-wide ones in `tasks/` as `mise run //:<task>`. A bare `mise run <task>` refers to the current directory's project. Run `mise tasks --all` to see the full list (build, test, format, lint, etc.).

Key things to know:

- Two-phase build: TS packages and libs in `packages/` compile first and get copied into `waspc/data/`, then Haskell (which embeds them). Use `mise run //waspc:build` for the full build. The TS builds declare `sources`/`outputs` and are skipped while up to date, so tasks that need them (e.g. `//waspc:test:e2e`) depend on them.
- Run the dev CLI with `wasp-cli <args>`, and the dev Wasp App Runner with `wasp-app-runner <args>`. Mise should put it on your path.
- Toolchain versions are specified in `mise.toml`.
- The repo is a pnpm workspace (`pnpm-workspace.yaml`). Only Wasp apps (`examples/`, e2e test apps) use npm, since Wasp requires it. mise reads the project graph for `^build` dependencies from it, and the root `mise.toml` adds what it can't infer (e.g. which packages `waspc` ships).

## Code Conventions

- When renaming a type or function, cascade the rename to parameters, local variables, and other names derived from it.

### Haskell

- Simple, readable Haskell — no complicated features. See `CONTRIBUTING.md`.
- Default extensions are listed in `waspc/waspc.cabal`.
- CamelCase for types/modules, camelCase for functions/values.
- Qualified imports preferred.
- Formatting: Ormolu (`mise run //waspc:check:ormolu` / `mise run //waspc:format:ormolu`). Linting: HLint (`mise run //waspc:hlint`, config in `waspc/.hlint.yaml`).
- Tests use `tasty` + `hspec` + `QuickCheck`, mirroring source module paths with a `Test` suffix.

### TypeScript/JavaScript

- Prettier-formatted (config in `prettier.config.mjs`). Check/fix with `mise run //:check:prettier` / `mise run //:format:prettier`.
- camelCase for files/functions, PascalCase for components/types.

### Architecture

- **Analyzer** reads the TypeScript config (`main.wasp.ts`) and Prisma schema and produces **AppSpec** (IR). **Generator** consumes AppSpec and produces React/Node.js code.
- Code generation uses a file draft system and Mustache templates in `data/Generator/templates/`.

## Important Rules

- **E2E snapshots** (`waspc/e2e-tests/test-outputs/snapshots/`) must never be manually edited. Regenerate them by running `mise run //waspc:test:e2e:accept-all`.
- **Documentation**: Only edit `web/docs/` (the latest version). Do not modify `web/versioned_docs/` — those are auto-generated snapshots of previous versions.
- **Markdown snapshots** (`web/markdown-snapshots/`) must never be manually edited. After changing docs, blog, or resources content (or the LLM files plugin), regenerate them by running `cd web && pnpm run build-dev && pnpm run markdown-snapshots:update`, then review the diff before committing. CI checks them with `pnpm run markdown-snapshots:check`.
- **Pull requests**: Always use the repo's `PULL_REQUEST_TEMPLATE.md`. Never delete any checkbox from the template — leave irrelevant ones unchecked.
