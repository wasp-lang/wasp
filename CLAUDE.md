# Wasp Monorepo

Wasp is a full-stack web framework that compiles TypeScript config (`main.wasp.ts`) files into React + Node.js applications. The compiler is written in Haskell.

## Repository Structure

- `waspc/` — Haskell compiler, CLI, and LSP server (the core of Wasp)
  - `src/` — Main compiler library (Analyzer, Generator, AppSpec, Psl)
  - `cli/src/` — CLI commands (start, build, new, deploy, etc.)
  - `data/packages/` — TypeScript packages called by the CLI when compiling projects as FFI
  - `data/Generator/libs/` — TypeScript libraries embedded into generated project code
  - `data/Generator/templates/` — Mustache templates for code generation
  - `e2e-tests/` — Golden file snapshot tests
  - `run` — **Main development script** (run `./run` with no args to see all commands)
- `wasp-app-runner/` — Node.js CLI for running Wasp apps in e2e tests
- `web/` — Documentation website (Docusaurus), deployed to wasp.sh
- `examples/` — Tutorial and example apps (kitchen-sink, waspello, etc.)
- `scripts/` — Monorepo-level build/packaging scripts

## Build & Development

All waspc development commands run from the `waspc/` directory via the `./run` script. Run `./run` with no arguments to see the full list of available commands (build, test, format, lint, etc.).

Key things to know:

- Two-phase build: TS packages in `data/packages/` and libs in `data/Generator/libs/` compile first, then Haskell (which embeds them). Use `./run build` for the full build.
- Run the dev CLI with `./run wasp-cli <args>`.
- Toolchain versions are specified in `mise.toml`.

## Code Conventions

### Haskell

- Simple, readable Haskell — no complicated features. See `CONTRIBUTING.md`.
- Default extensions are listed in `waspc/waspc.cabal`.
- CamelCase for types/modules, camelCase for functions/values.
- Qualified imports preferred.
- Formatting: Ormolu (`./run check:ormolu` / `./run format:ormolu`). Linting: HLint (`./run hlint`, config in `waspc/.hlint.yaml`).
- Tests use `tasty` + `hspec` + `QuickCheck`, mirroring source module paths with a `Test` suffix.

### TypeScript/JavaScript

- Prettier-formatted (config in `prettier.config.mjs`). Check/fix with `./run check:prettier` / `./run format:prettier`.
- camelCase for files/functions, PascalCase for components/types.

### Architecture

- TypeScript config (`main.wasp.ts`) is read by `Wasp.Project.WaspFile.TypeScript` → **AppSpec** (IR) → **Generator** produces React/Node.js code. The **Analyzer** derives entity declarations from the Prisma schema.
- Code generation uses a file draft system and Mustache templates in `data/Generator/templates/`.

## Important Rules

- **E2E snapshots** (`waspc/e2e-tests/test-outputs/snapshots/`) must never be manually edited. Regenerate them by running `cd waspc && ./run build && ./run test:waspc:e2e:accept-all`.
- **Documentation**: Only edit `web/docs/` (the latest version). Do not modify `web/versioned_docs/` — those are auto-generated snapshots of previous versions.
- **Pull requests**: Always use the repo's `PULL_REQUEST_TEMPLATE.md`. Never delete any checkbox from the template — leave irrelevant ones unchecked.
