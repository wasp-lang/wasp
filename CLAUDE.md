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
- `nix/` — **Build system and dev tooling** (Nix flake modules: toolchain, dev commands, packaging)
- `wasp-app-runner/` — Node.js CLI for running Wasp apps in e2e tests
- `web/` — Documentation website (Docusaurus), deployed to wasp.sh
- `examples/` — Tutorial and example apps (kitchen-sink, waspello, etc.)
- `scripts/` — Monorepo-level build/packaging scripts

## Build & Development

All development commands are Nix flake apps, runnable from anywhere inside the repo as `nix run .#<command>` (build, test, format, lint, etc.). They are defined in `nix/apps.nix`; `nix flake show` lists them all. `nix develop` enters a shell with all dev tools (GHC, cabal, node, formatters).

Key things to know:

- Two-phase build: TS packages in `data/packages/` and libs in `data/Generator/libs/` compile first, then Haskell (which embeds them). Use `nix run .#build` for the full build.
- Run the dev CLI with `nix run .#wasp-cli -- <args>`.
- Toolchain versions are specified in the Nix flake (`nix/toolchain.nix`).
- `nix build .#wasp-cli` builds the fully-packaged CLI; release tarballs come from `nix/release.nix`.

## Code Conventions

### Haskell

- Simple, readable Haskell — no complicated features. See `CONTRIBUTING.md`.
- Default extensions are listed in `waspc/waspc.cabal`.
- CamelCase for types/modules, camelCase for functions/values.
- Qualified imports preferred.
- Formatting: Ormolu (`nix run .#check-ormolu` / `nix run .#format-ormolu`). Linting: HLint (`nix run .#hlint`, config in `waspc/.hlint.yaml`).
- Tests use `tasty` + `hspec` + `QuickCheck`, mirroring source module paths with a `Test` suffix.

### TypeScript/JavaScript

- Prettier-formatted (config in `prettier.config.mjs`). Check/fix with `nix run .#check-prettier` / `nix run .#format-prettier`.
- camelCase for files/functions, PascalCase for components/types.

### Architecture

- TypeScript config (`main.wasp.ts`) is read by `Wasp.Project.WaspFile.TypeScript` → **AppSpec** (IR) → **Generator** produces React/Node.js code. The **Analyzer** derives entity declarations from the Prisma schema.
- Code generation uses a file draft system and Mustache templates in `data/Generator/templates/`.

## Important Rules

- **E2E snapshots** (`waspc/e2e-tests/test-outputs/snapshots/`) must never be manually edited. Regenerate them by running `nix run .#build && nix run .#test-waspc-e2e-accept-all`.
- **Documentation**: Only edit `web/docs/` (the latest version). Do not modify `web/versioned_docs/` — those are auto-generated snapshots of previous versions.
- **Pull requests**: Always use the repo's `PULL_REQUEST_TEMPLATE.md`. Never delete any checkbox from the template — leave irrelevant ones unchecked.
