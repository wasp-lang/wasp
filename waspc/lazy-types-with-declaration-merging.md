## Lazy Types with Declaration Merging

### How we fetch user types

Wasp uses TypeScript module augmentation to bring user-defined types into the SDK. Empty registry interfaces are declared in `wasp/types`, and generated `declare module` blocks merge user types into them at build time:

```typescript
// SDK: empty registry
export interface CrudOverridesRegistry {}

// Generated augmentation (from user's code):
declare module "wasp/types" {
  interface CrudOverridesRegistry {
    tasks: {
      GetAll: typeof import("./tasks").getAllOverride;
    };
  }
}
```

Conditional types resolve the override or fall back to a default:

```typescript
type GetAllQueryResolved = FromCrudOverridesRegistry<
  "tasks",
  "GetAll",
  DefaultGetAllQuery
>;
// → user's override if augmented, DefaultGetAllQuery otherwise
```

### Why types must stay lazy

The SDK is compiled with `tsc` to produce `.d.ts` declaration files, which consumer code (user's app) then imports. TypeScript's declaration emitter handles inferred vs annotated types differently:

**With explicit annotation** — the emitted `.d.ts` preserves the type alias:

```typescript
// Source
const _getAllQuery: QueryFor<GetAllQueryResolved> = createQuery<GetAllQueryResolved>(...)

// Emitted .d.ts
declare const _getAllQuery: QueryFor<GetAllQueryResolved>;
```

The consumer resolves `GetAllQueryResolved` against the augmented `CrudOverridesRegistry`, picking up the user's override.

**Without annotation** — the emitter expands the type into its full structural form:

```typescript
// Source
const _getAllQuery = createQuery<GetAllQueryResolved>(...)

// Emitted .d.ts (conditionals already resolved to defaults)
declare const _getAllQuery: {
  (args: {}): Promise<{ id: number; description: string; userId: number }[]>;
  queryCacheKey: string[];
};
```

The declaration emitter asks the type checker for the fully inferred type of the variable. Since the variable is unannotated, the checker evaluates the conditional using only the types visible during SDK compilation. The override is lost — the consumer sees only the default Prisma types.

Note: source type-checking (`tsc --noEmit`) works correctly in both cases — the issue is specifically in declaration emit. The SDK builds first, then the consumer imports the emitted `.d.ts` files. This two-phase compilation is why the timing of type resolution matters.

### Two approaches to lazy types

**Generic function defaults** (TanStack Router's approach):

```typescript
function useLoaderData<TRouter = RegisteredRouter>() { ... }
```

Generics must remain abstract in emitted declarations, so the emitter preserves them rather than resolving them. Each call site resolves the default against its own augmentation context. This pattern works naturally for APIs that resolve types at function call sites, but not for stored values.

**Explicit type annotations** (Wasp's approach):

Wasp exposes value-based APIs — `env.MY_VAR`, `tasks.get.query`, `dbClient` — where types must be stored in variables. Explicit annotations force the declaration emitter to preserve the type alias references rather than expanding them:

```typescript
const _getQuery: QueryFor<GetQueryResolved> = createQuery<GetQueryResolved>(...)
export const env: ClientEnv = _env
export const dbClient: UserPrismaClient = setupFn()
```

Even when the annotation appears redundant (e.g., `createQuery<T>` returns `QueryFor<T>`), removing it causes the declaration emitter to expand the type, breaking override resolution for consumers.

Since all annotations live in generated template code, this cost is invisible to users.
