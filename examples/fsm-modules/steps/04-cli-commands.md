# Phase 4: CLI Commands

`wasp module build`, `wasp module build --watch`, and `wasp module init`.

## Goal

Module authors interact with the compiler via `wasp module` subcommands. These are the primary entry points for the module development workflow.

## Files to Create/Modify

| File | Change |
|---|---|
| New: `Wasp/Cli/Command/Module.hs` | `wasp module build`, `wasp module init` |
| `Wasp/Cli/Command.hs` (or main CLI router) | Register `module` subcommand group |

## Commands

### `wasp module init [name]`

Scaffolds a new module project:

```
@myorg/my-module/
├── module.wasp.ts
├── src/
│   └── index.ts
├── package.json
└── tsconfig.json
```

See Phase 7 for scaffold template details.

### `wasp module build`

One-shot build pipeline (see Phase 5 for full details):

1. Find `module.wasp.ts`
2. Compile to JS via `tsc`
3. Execute via `wasp-config` CLI (module mode)
4. Extract module spec
5. Generate synthetic Prisma schema
6. Run `prisma generate`
7. Generate module SDK
8. Run `tsc --noEmit` to verify module code compiles

### `wasp module build --watch`

Same pipeline but watches `module.wasp.ts` and `src/` for changes, re-running the pipeline on save.

## CLI Structure

```
wasp module init [name]          # scaffold module project
wasp module build                # one-shot build
wasp module build --watch        # watch mode
```

## Verification

- `wasp module init @test/my-mod` creates the expected file structure
- `wasp module build` runs the full pipeline and exits 0 on a valid module
- `wasp module build --watch` watches for changes and re-generates
- Running `wasp module build` in an app project (with `main.wasp.ts`) errors with a clear message
