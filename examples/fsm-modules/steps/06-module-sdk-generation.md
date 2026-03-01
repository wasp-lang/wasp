# Phase 6: Module SDK Generation

Generate the SDK files that let module code import from `wasp/*` paths.

## Goal

After `wasp module build`, all standard module imports resolve:

```typescript
// Server
import type { AuthenticatedQuery } from "wasp/server/module";
import { HttpError } from "wasp/server/module";
import type { Todo } from "wasp/entities";

// Client
import { useQuery, useAction } from "wasp/client/operations";
import { getSubscription, createCheckout } from "wasp/client/operations";

// Config
import { config } from "wasp/modules/config";
```

## Generated File Tree

```
.wasp/out/sdk/wasp/
├── entities/
│   └── index.ts              # Re-exports Prisma types from @prisma/client
├── server/
│   ├── index.ts              # HttpError re-export
│   ├── module/
│   │   └── index.ts          # Re-exports from wasp-config/module
│   └── operations/
│       ├── queries/types.ts  # Per-query context types (entities + auth)
│       └── actions/types.ts  # Per-action context types
├── client/
│   └── operations/
│       ├── index.ts          # All module operations re-exported
│       ├── queries/
│       │   ├── core.ts       # createQuery, QueryFor, useQuery
│       │   └── index.ts      # Module query exports
│       └── actions/
│           ├── core.ts       # createAction, ActionFor, useAction
│           └── index.ts      # Module action exports
├── modules/
│   └── config.ts             # Module's provide() values
└── package.json              # SDK package with wasp/* exports
```

## Files to Create/Modify

| File | Change |
|---|---|
| New: `Wasp/Generator/ModuleGenerator/SdkGenerator.hs` | Module SDK generation orchestrator |
| `Wasp/Generator/SdkGenerator/Server/OperationsGenerator.hs` | Use entity aliases for module operations |
| `Wasp/Generator/SdkGenerator/Client/OperationsGenerator.hs` | Generate module client operations |
| New: SDK templates for each generated file | Mustache or TH templates |

## Details

### `wasp/entities/index.ts`

Re-exports Prisma model types from the generated `@prisma/client`:

```typescript
export type { Todo, Subscription } from "@prisma/client";
```

One export per entity declaration in the module.

### `wasp/server/module/index.ts`

Re-exports runtime types from `wasp-config/module` (Phase 2):

```typescript
export {
  type PrismaDelegate,
  type OperationContext,
  type AuthOperationContext,
  type Query,
  type AuthenticatedQuery,
  type Action,
  type AuthenticatedAction,
  type AuthUser,
  HttpError,
} from "wasp-config/module";
```

### `wasp/server/operations/queries/types.ts`

Per-query type definitions with the correct entity context:

```typescript
import type { Todo } from "wasp/entities";
import type { AuthenticatedQuery } from "wasp/server/module";

export type GetTodos = AuthenticatedQuery<{ Todo: Todo }, void, Todo[]>;
```

### `wasp/client/operations/index.ts`

Client-side operation hooks and functions. Module operations are generated similarly to app-mode operations but scoped to the module's declared operations.

### `wasp/modules/config.ts`

Exports the module's `provide()` values:

```typescript
export const config = {
  webhookPath: "/stripe-webhook",
  // ... other provided values
};
```

### `package.json`

The SDK `package.json` declares exports for all `wasp/*` paths so TypeScript resolves them.

## What's NOT Generated (Module Mode)

- No Express server setup
- No web app / Vite config
- No auth forms, OAuth providers, email senders
- No Docker files
- No database migrations
- No route/page generation

## Verification

- `import type { Todo } from "wasp/entities"` resolves to the Prisma-generated type
- `import { HttpError } from "wasp/server/module"` is a real class, not just a type
- Client operation hooks (`useQuery`, `useAction`) are importable
- `tsc --noEmit` passes on the module's source code after SDK generation
