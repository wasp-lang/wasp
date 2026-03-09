# Wasp Monorepo

Wasp is a full-stack web framework that compiles `.wasp` configuration files into React + Node.js applications. The compiler is written in Haskell.

## Repository Structure

- `waspc/` — Haskell compiler, CLI, and LSP server (the core of Wasp)
  - `src/` — Main compiler library (Analyzer, Generator, AppSpec, Psl)
  - `cli/src/` — CLI commands (start, build, new, deploy, etc.)
  - `waspls/` — Language Server Protocol implementation
  - `data/packages/` — TypeScript packages embedded in the compiler (wasp-config, prisma, ts-inspect, studio, deploy)
  - `data/Generator/templates/` — Mustache templates for code generation
  - `e2e-tests/` — Golden file snapshot tests
  - `run` — **Main development script** (use `./run <command>` for everything)
- `wasp-app-runner/` — Node.js CLI for running Wasp apps in e2e tests
- `web/` — Documentation website (Docusaurus), deployed to wasp.sh
- `mage/` — Example Wasp showcase app (Vite + React)
- `examples/` — Tutorial and example apps (kitchen-sink, waspello, etc.)
- `scripts/` — Monorepo-level build/packaging scripts

## Build & Development

All waspc commands run from the `waspc/` directory using the `./run` script.

### Building
```bash
./run build          # Build everything (TS packages + Haskell)
./run build:hs       # Build Haskell only
./run build:packages # Build TypeScript packages only
./run build:libs     # Build Wasp libs only
```

### Running
```bash
./run wasp-cli <args>  # Run the dev CLI (builds first if needed)
./run install          # Install globally as 'wasp-cli'
```

### Testing
```bash
./run test                       # ALL tests (unit + e2e + examples + starters + libs)
./run test:waspc:unit [pattern]  # Waspc unit tests (tasty pattern matching)
./run test:waspc:e2e             # E2E golden file tests
./run test:waspc:e2e:accept-all  # Accept all snapshot changes
./run test:cli                   # CLI tests
./run test:waspls                # LSP tests
./run test:libs                  # TypeScript lib tests
./run test:kitchen-sink          # Kitchen sink example e2e
./run test:examples              # All examples e2e
./run test:starters              # Starter template e2e
```

### Formatting & Linting
```bash
./run check            # Check all formatting (ormolu + cabal-gild + prettier)
./run format           # Fix all formatting
./run check:ormolu     # Check Haskell formatting
./run format:ormolu    # Fix Haskell formatting
./run check:cabal      # Check .cabal formatting
./run format:cabal     # Fix .cabal formatting
./run check:prettier   # Check JS/TS/JSON/YAML formatting
./run format:prettier  # Fix JS/TS/JSON/YAML formatting
./run hlint            # Haskell linting
./run stan             # Static analysis
```

### Dev Watch
```bash
./run ghcid       # Watch source files for errors
./run ghcid-test  # Watch source + tests
```

## Toolchain

- **GHC**: 9.6.7 (set via `./run ghcup-set`)
- **HLS**: 2.10.0.0
- **Node.js**: v22.12.0 minimum (see `.nvmrc`)
- **Haskell formatter**: Ormolu (v0.7.2.0)
- **Haskell linter**: HLint (v3.6.1, config in `.hlint.yaml`)
- **Import formatter**: Stylish-Haskell (`.stylish-haskell.yaml`)
- **JS/TS formatter**: Prettier 3.5.3 with plugins (organize-imports, pkg, sh, tailwindcss)
- **Cabal formatter**: cabal-gild (v1.5.0.1)

## Code Conventions

### Haskell
- Simple, readable Haskell — no complicated features
- Default extensions: `OverloadedStrings`, `LambdaCase`, `ScopedTypeVariables`, `QuasiQuotes`, `TemplateHaskell`, etc. (see `waspc.cabal`)
- CamelCase for types/modules, camelCase for functions/values
- Qualified imports preferred
- Ormolu-formatted, HLint-checked
- Tests use `tasty` + `hspec` + `QuickCheck`, organized by module name with `Test` suffix

### TypeScript/JavaScript
- Prettier-formatted: double quotes, trailing commas, semicolons, 2-space indent
- camelCase for files/functions, PascalCase for components/types

### Architecture
- **Analyzer** parses `.wasp` files → **AppSpec** (AST) → **Generator** produces React/Node.js code
- Code generation uses a file draft system (TextFileDraft, TemplateFileDraft, etc.)
- Templates use Mustache syntax in `data/Generator/templates/`

## CI

CI runs on push to main/release and on PRs. Key checks:
- Formatting (Prettier + Ormolu + cabal-gild)
- Build on Linux (x86_64, static), macOS (x86_64, aarch64)
- Tests on Ubuntu, macOS (Intel + Silicon), Windows with multiple Node versions
- Example apps and starter template e2e tests
- TypeScript lib tests
- NPM package build/test/publish

## Important Notes
- Two-phase build: TS packages compile first, then Haskell (which embeds them)
- **E2E snapshots** (`waspc/e2e-tests/test-outputs/snapshots/`) must never be manually edited. Regenerate them by running `cd waspc && ./run build && ./run test:waspc:e2e:accept-all`.
- **Documentation**: Only edit `web/docs/` (the latest version). Do not modify `web/versioned_docs/` — those are auto-generated snapshots of previous versions.
- **Pull requests**: Always use the repo's `PULL_REQUEST_TEMPLATE.md`. Never delete any checkbox from the template — leave irrelevant ones unchecked.
- Version is sourced from `waspc.cabal`
