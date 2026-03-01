# Phase 2: wasp-config/module Runtime Types

Publish `PrismaDelegate`, `OperationContext`, `AuthUser`, and `HttpError` from `wasp-config`.

## Goal

Module authors can type-check their server code against these types **without** running the compiler or having a host app. These types live in `wasp-config/module` and are re-exported by the generated SDK's `wasp/server/module/index.ts`.

## Files to Create/Modify

| File | Change |
|---|---|
| New: `src/module.ts` (or `src/publicApi/module.ts`) | All runtime types |
| `package.json` | Add `"./module"` export |
| Template: `sdk/wasp/server/module/index.ts` | Re-export from `wasp-config/module` |

## Types to Export

```typescript
// wasp-config/module

export interface PrismaDelegate<T = any> {
  findMany(args?: any): Promise<T[]>;
  findUnique(args: any): Promise<T | null>;
  findUniqueOrThrow(args: any): Promise<T>;
  findFirst(args?: any): Promise<T | null>;
  findFirstOrThrow(args?: any): Promise<T>;
  create(args: any): Promise<T>;
  createMany(args: any): Promise<{ count: number }>;
  update(args: any): Promise<T>;
  updateMany(args: any): Promise<{ count: number }>;
  upsert(args: any): Promise<T>;
  delete(args: any): Promise<T>;
  deleteMany(args?: any): Promise<{ count: number }>;
  count(args?: any): Promise<number>;
}

export type OperationContext<E extends Record<string, any>> = {
  entities: { [K in keyof E]: PrismaDelegate<E[K]> };
};

export type AuthOperationContext<E extends Record<string, any>> =
  OperationContext<E> & { user?: AuthUser };

export type Query<E, Input, Output> = ...
export type AuthenticatedQuery<E, Input, Output> = ...
export type Action<E, Input, Output> = ...
export type AuthenticatedAction<E, Input, Output> = ...

export type AuthUser = {
  id: number | string;
  identities: Record<string, { id: string } | null>;
  getFirstProviderUserId: () => string | null;
  [key: string]: unknown;
};

export class HttpError extends Error {
  statusCode: number;
  data: Record<string, any>;
  constructor(statusCode: number, message?: string, data?: Record<string, any>);
}
```

## SDK Re-export

In module dev mode, `wasp/server/module/index.ts` is:

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

In host app mode, the generated SDK provides app-specific concrete types instead (e.g., real `AuthUser` from auth config).

## Verification

- Module code like `import type { AuthenticatedQuery } from "wasp/server/module"` resolves
- `HttpError` can be instantiated: `throw new HttpError(404, "Not found")`
- Types work without running the compiler (standalone `tsc`)
