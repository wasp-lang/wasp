# Phase 5: `wasp module build` Pipeline

The core build pipeline that turns `module.wasp.ts` into a type-checkable module SDK.

## Goal

`wasp module build` takes a `module.wasp.ts`, generates a synthetic Prisma schema + module SDK, and verifies the module code compiles standalone.

## Pipeline Steps

```
module.wasp.ts
  │
  ├─1─► tsc compile to JS
  ├─2─► wasp-config CLI execute (module mode) → ModuleSpec JSON
  ├─3─► Haskell parses ModuleSpec JSON
  ├─4─► Generate synthetic schema.prisma
  ├─5─► prisma generate (types only, no DB)
  ├─6─► Generate module SDK files
  └─7─► tsc --noEmit (verify module code compiles)
```

## Details

### Step 1-3: Evaluate `module.wasp.ts`

Reuses the existing `wasp-config` evaluation pipeline. The `run.ts` branching (Phase 1) detects a `Module` default export and outputs the module spec as JSON.

### Step 4: Synthetic Prisma Schema

From entity declarations, generate `.wasp/out/db/schema.prisma`:

```prisma
datasource db {
  provider = "sqlite"
  url      = "file:./module-dev.db"
}

generator client {
  provider = "prisma-client-js"
}

model Todo {
  id        Int      @id @default(autoincrement())
  text      String
  isDone    Boolean  @default(false)
  createdAt DateTime @default(now())
}
```

Key points:
- SQLite provider (lightweight, no external DB needed)
- Each entity declaration becomes a standalone model
- Field strings are emitted as-is (they're valid Prisma syntax)
- No relations between models

### Step 5: `prisma generate`

Run `prisma generate` against the synthetic schema. This only reads the schema file — no database connection needed. Produces `@prisma/client` types for all declared entities.

### Step 6: Module SDK Generation

See Phase 6 for the full SDK file list and generation details.

### Step 7: Type Verification

Run `tsc --noEmit` against the module's `tsconfig.json` to verify all imports resolve and types check out.

## Files to Create/Modify

| File | Change |
|---|---|
| New: `Wasp/Generator/ModuleGenerator.hs` | Orchestrates the module build pipeline |
| New: `Wasp/Generator/ModuleGenerator/DbGenerator.hs` | Synthetic `schema.prisma` generation |

## Verification

- A valid `module.wasp.ts` with entity declarations produces a correct `schema.prisma`
- `prisma generate` succeeds on the synthetic schema
- Module code with `import type { Todo } from "wasp/entities"` compiles
- The pipeline exits 0 for a correct module, non-zero with clear errors for invalid modules
