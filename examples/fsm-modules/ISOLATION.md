# Interface-Based Module Isolation for Wasp FSM — Research & Design

## Context

The current FSM prototype has modules that leak into the host app's SDK:
- Entity references are untyped strings (`entities?: string[]`, `context.entities[todoEntityName]`)
- Module code imports from the app-generated SDK (`wasp/server/module`, `wasp/server`)
- Runtime config uses unsafe casts (`config.get(PACKAGE_NAME)! as RuntimeConfig`)
- Modules can't develop in true isolation — they need a dev app to compile against

The goal: **modules declare interfaces, develop in isolation, and the compiler generates glue code that satisfies those interfaces.**

```
Module → Interface (declares needs) → Compiler (validates + generates) → Host app (satisfies)
```

---

## Part 1: External Research

### Framework comparison

| Framework | Isolation model | Contract mechanism | Entity handling |
|---|---|---|---|
| **MedusaJS v2** | Strict: each module owns its own DI container | Services declare `InjectedDependencies` | Modules own their own data models; cross-module links at app level |
| **Shopify** | Sandbox: extensions run in isolated context | Declarative TOML config (targets + capabilities + metafields) | Platform provides data via declared metafield keys |
| **NestJS** | Module-scoped providers, exports for sharing | `forRoot(options)` with injection tokens | No entity handling (service-level DI only) |
| **Spring Boot** | Starters define interfaces + defaults | `@ConditionalOnMissingBean` for overridable defaults | Interfaces for repositories, host provides implementations |
| **Elixir** | `@behaviour` for compile-time module contracts | Callbacks with type specs, compiler warns on missing | N/A |

**Key takeaway**: MedusaJS and Shopify are closest to what we want. MedusaJS avoids the entity interface problem entirely (modules own their own data, cross-module links are declared at the app level). Shopify uses declarative TOML as an interface contract validated at deploy time. Spring Boot's `@ConditionalOnMissingBean` maps to "module needs X, host can provide its own". Elixir behaviours are the gold standard for compile-time contract validation.

### TypeScript patterns

- **Structural typing**: TS already works like Go interfaces — `{ email: string; id: number }` is satisfied by any object with at least those fields.
- **Generic type parameters**: `createModule<TUser extends HasEmail>()` enforces contracts via `extends`. But can't validate against Prisma schema at the TS level (Prisma types aren't generated when `main.wasp.ts` runs).
- **Schema-driven codegen** (Prisma/GraphQL/protobuf): One definition file generates both client and server code. The module's interface declaration would serve the same role.

### Client-side type flow patterns

- **tRPC**: Exports `type AppRouter = typeof appRouter`, client parameterized with `createTRPCClient<AppRouter>()`. Types flow via inference, no codegen.
- **TanStack Query v5**: `queryOptions()` creates a typed reference object. `useQuery(queryOptions(...))` infers return types automatically. Wasp's `createQuery<BackendQuery>()` is the same pattern.
- **React Router v7**: Uses a `typegen` step generating `.d.ts` files, with `rootDirs` config to make them appear co-located. Codegen approach, not applicable to shipped npm packages.
- **Node.js `#` subpath imports**: Package-scoped imports (`#module/operations`) that resolve differently in dev vs production. Standard Node.js feature, well-supported.

---

## Part 2: How the Wasp Compiler Currently Works (Relevant Parts)

### Operation code generation data flow

For a query `getTodos` using entity `Task`:

```
1. Haskell reads AppSpec:
   Operation { name="getTodos", fn=ExtImport "@src/queries", entities=[Ref "Task"] }

2. makeJsonWithEntityData produces:
   { "name": "Task", "internalTypeName": "_Task", "prismaIdentifier": "task" }

3. Server template (_query.ts) generates:
   export default async function(args, context) {
     return getTodos_ext(args, { ...context, entities: { Task: prisma.task } })
   }

4. SDK server operations template generates:
   export const getTodos: UnauthenticatedOperationFor<GetTodos> =
     createUnauthenticatedOperation(getTodos_ext, { Task: prisma.task })

5. SDK client operations template generates:
   export const getTodos: QueryFor<GetTodos_ext> = createQuery<GetTodos_ext>(
     'operations/get-todos', ['Task']
   )

6. useQuery(getTodos) → data typed as Todo[] via OperationRpcFor<> type extraction
```

### Path rewriting

- `@src/` paths → `ExtImportSrcPath` → resolved to `wasp/ext-src/...` in generated code
- `@pkg/` paths → `ExtImportPkgPath` → emitted verbatim as `RawModuleImportPath`
- `App.use()` rewrites module `@src/` → `@pkg/{packageName}/` when `packageName ≠ App.projectPackageName`

### Key Haskell types and functions

- `makeJsonWithEntityData` (`Generator/Common.hs`): Produces `{name, internalTypeName, prismaIdentifier}` for template data
- `operationTmplData` (`ServerGenerator/OperationsG.hs`): Builds template JSON including `jsFn` and `entities`
- `getQueryData` (`SdkGenerator/Client/OperationsGenerator.hs`): Builds client query data including `queryRoute`, `entitiesArray`, `operationTypeName`
- `extImportToJsImport` (`SdkGenerator/JsImport.hs`): Converts `ExtImport` to `JsImport` with `RawModuleImportPath` for `@pkg/` paths

### Current module-specific code

- `wasp/server/module/index.ts`: Provides `PrismaDelegate<T=any>`, `OperationContext`, `AuthUser`, and simplified operation definition types with 2 generics instead of 3 (modules don't know app entities)
- `wasp/modules/config.ts`: Generated `Map<packageName, values>` from `provide()` calls
- `App.ts:use()`: Merges module declarations, rewrites paths, collects setup functions and provides

---

## Part 3: Proposed Design

### 3.1 Module declares its interface

Two approaches explored (both should be considered):

#### Approach A: Field-level entity declarations (full structural validation)

```typescript
// @waspello/todo-module/src/index.ts
import { Module } from "wasp-config";

export function createTodoModule(config: TodoModuleConfig): Module {
  const mod = new Module("@waspello/todo-module");

  // Declare entity requirements with Prisma types
  mod.entity("Todo", {
    fields: {
      id:     "Int",
      text:   "String",
      isDone: "Boolean",
    }
  });

  mod.requiresAuth();

  mod.query("getTodos", {
    fn: { import: "getTodos", from: "@src/queries" },
    entities: ["Todo"],    // references declared alias, not host name
    auth: true,
  });

  return mod;
}
```

**Pros**: Compiler validates that host entities have matching fields/types. Clear error messages ("Module X requires field `isDone: Boolean` but entity `TodoItem` doesn't have it"). Catches mismatches at compile time.

**Cons**: More ceremony for module authors. Duplication between `mod.entity()` declaration and the module's own TS types. Need to handle Prisma modifiers (`@default`, `@id`, `@unique`, `@relation`).

**Compiler work**: Haskell receives entity requirement declarations as JSON. Validates each required field exists in the mapped host entity's Prisma schema AST with compatible type. The Prisma parser already exists in `waspc`.

#### Approach B: Alias-only entities (TypeScript-level safety only)

```typescript
const mod = new Module("@waspello/todo-module");

mod.entity("Todo");  // just declares the alias, no field requirements

mod.query("getTodos", {
  fn: { import: "getTodos", from: "@src/queries" },
  entities: ["Todo"],
  auth: true,
});
```

Module author writes their own TS types and relies on TypeScript structural typing for safety:

```typescript
// Module code — fully typed via generic parameter
type Todo = { id: number; text: string; isDone: boolean };
type Entities = { Todo: Todo };

export const getTodos: AuthenticatedQuery<Entities, void, Todo[]> = async (_args, ctx) => {
  return ctx.entities.Todo.findMany({ ... });
};
```

**Pros**: Less ceremony. Single source of truth for types (the module's TS types). Simpler compiler. Module authors are already writing TS types anyway.

**Cons**: No compile-time validation against host schema. Mismatches surface as runtime errors ("Cannot read property 'isDone' of undefined"). Less safety than Approach A.

**Compiler work**: Minimal — just needs to track entity aliases and generate the entity map.

#### Recommendation

Start with **Approach B** for faster iteration, with the architecture designed to support **Approach A** later. The `mod.entity("Todo")` API should accept an optional `{ fields }` parameter from the start so the path to Approach A is clear.

### 3.2 Module code uses typed entity access

Module operations import generic types from `wasp-config` (or a future `wasp-module-sdk`):

```typescript
// @waspello/todo-module/src/queries.ts
import type { AuthenticatedQuery } from "wasp-config/module";
import { HttpError } from "wasp-config/module";

type Todo = { id: number; text: string; isDone: boolean };
type Entities = { Todo: Todo };

export const getTodos: AuthenticatedQuery<Entities, void, Todo[]> = async (
  _args, context,
) => {
  // TYPED: context.entities.Todo is PrismaDelegate<Todo>
  return context.entities.Todo.findMany({
    where: context.user ? { userId: context.user.id } : undefined,
    orderBy: { id: "asc" },
  });
};
```

**What changes from current `wasp/server/module`:**

| Current | Proposed |
|---|---|
| `OperationContext = { entities: Record<string, PrismaDelegate> }` | `OperationContext<E> = { entities: { [K in keyof E]: PrismaDelegate<E[K]> } }` |
| `AuthenticatedQueryDefinition<Input, Output>` (2 generics) | `AuthenticatedQuery<Entities, Input, Output>` (3 generics, entities typed) |
| `context.entities[todoEntityName]` (string lookup, `PrismaDelegate<any>`) | `context.entities.Todo` (property access, `PrismaDelegate<Todo>`) |
| Import from `wasp/server/module` (app-generated SDK) | Import from `wasp-config/module` (published npm package) |

### 3.3 Host app maps entities

```typescript
// waspello/main.wasp.ts
app.use(createTodoModule({ route: "/todos" }), {
  entityMap: {
    Todo: "TodoItem",   // module's "Todo" → host's "TodoItem" entity
  },
});
```

The `entityMap` is the second argument to `app.use()`, separate from the module's own config. This makes the mapping explicit and owned by the host.

### 3.4 Client-side: `#module/operations` with compiler codegen

This is the approach that achieves **type-safe + great DX** for module React components.

#### What the module author writes

```typescript
// @waspello/todo-module/src/TodoPage.tsx
import { getTodos, createTodo } from '#module/operations';
import { useQuery, useAction } from 'wasp/client/operations';

export default function TodoPage() {
  const { data: todos } = useQuery(getTodos);   // todos: Todo[] — fully typed
  const create = useAction(createTodo);          // typed input/output
  // ...
}
```

`#module/operations` is a [Node.js subpath import](https://nodejs.org/api/packages.html#subpath-imports) — package-scoped, standard, well-supported.

#### How it resolves during standalone development

The module provides stubs that the `#module/*` imports resolve to:

```typescript
// @waspello/todo-module/src/_stubs/operations.ts
// During standalone dev, these re-export from the app-generated SDK
export { getTodos } from 'wasp/client/operations';
export { createTodo } from 'wasp/client/operations';
```

```jsonc
// package.json (Node.js subpath imports)
{ "imports": { "#module/*": "./src/_stubs/*.js" } }

// tsconfig.json (TypeScript resolution)
{ "compilerOptions": { "paths": { "#module/*": ["./src/_stubs/*"] } } }
```

#### What the compiler generates when consumed by a host app

```typescript
// Generated: sdk/wasp/client/operations/modules/@waspello/todo-module.ts
import { createQuery } from '../queries/core';
import { createAction } from '../actions/core';
import type { getTodos as GetTodos_fn } from '@waspello/todo-module/queries';
import type { createTodo as CreateTodo_fn } from '@waspello/todo-module/actions';

export const getTodos: QueryFor<typeof GetTodos_fn> = createQuery<typeof GetTodos_fn>(
  'operations/get-todos',
  ['TodoItem'],   // actual host entity names for cache invalidation
);

export const createTodo: ActionFor<typeof CreateTodo_fn> = createAction<typeof CreateTodo_fn>(
  'operations/create-todo',
  ['TodoItem'],
);
```

The `App.use()` path rewriting maps `#module/operations` in the compiled module to this generated file, following the same `@src/` → `@pkg/` pattern.

#### Why this works

- **Type-safe**: Types flow from server function signatures through `QueryFor<>`/`ActionFor<>` → `OperationRpcFor<>` extracts `Input`/`Output` → `useQuery(getTodos)` infers `Todo[]`. Exact same mechanism as app-level operations.
- **Great DX**: `import { getTodos } from '#module/operations'` — one line, full autocomplete, no props drilling or manual type annotations.
- **Isolated**: `#module/*` is package-scoped (Node.js spec). Module never directly imports from `wasp/client/operations`. Stubs are for dev mode only.
- **Minimal compiler changes**: Extends existing `@src/` → `@pkg/` rewriting. Generator produces one extra file per module using the same `createQuery`/`createAction` templates.

### 3.5 Module SDK types

These types would live in `wasp-config/module` (new export from existing package) or a separate `wasp-module-sdk`:

```typescript
// Generic PrismaDelegate — same as current wasp/server/module but typed
export interface PrismaDelegate<T = any> {
  findMany(args?: any): Promise<T[]>;
  findUnique(args: any): Promise<T | null>;
  findUniqueOrThrow(args: any): Promise<T>;
  create(args: any): Promise<T>;
  update(args: any): Promise<T>;
  delete(args: any): Promise<T>;
  count(args?: any): Promise<number>;
  // ...full Prisma delegate interface
}

// Typed operation context (3 generics instead of current 2)
export type OperationContext<E extends Record<string, any>> = {
  entities: { [K in keyof E]: PrismaDelegate<E[K]> };
};

export type AuthOperationContext<E extends Record<string, any>> =
  OperationContext<E> & { user?: AuthUser };

export type Query<E extends Record<string, any>, Input, Output> =
  (args: Input, context: OperationContext<E>) => Output | Promise<Output>;

export type AuthenticatedQuery<E extends Record<string, any>, Input, Output> =
  (args: Input, context: AuthOperationContext<E>) => Output | Promise<Output>;

export type Action<E extends Record<string, any>, Input, Output> =
  (args: Input, context: OperationContext<E>) => Output | Promise<Output>;

export type AuthenticatedAction<E extends Record<string, any>, Input, Output> =
  (args: Input, context: AuthOperationContext<E>) => Output | Promise<Output>;

// Auth user — generic, not app-specific
export type AuthUser = {
  id: number | string;
  identities: Record<string, { id: string } | null>;
  getFirstProviderUserId: () => string | null;
  [key: string]: unknown;
};

// Core utilities
export class HttpError extends Error { ... }
```

### 3.6 What the compiler generates (detailed)

#### Server-side entity adapter (new template)

For each module with entity mappings, generate an entity adapter:

```mustache
{{={= =}=}}
import { prisma } from 'wasp/server'

export const moduleEntities = {
  {=# entityMappings =}
  {= alias =}: prisma.{= realPrismaIdentifier =},
  {=/ entityMappings =}
} as const
```

Template data from Haskell:
```json
{
  "entityMappings": [
    { "alias": "Todo", "realPrismaIdentifier": "todoItem" }
  ]
}
```

#### Server operation wrappers (modified existing template)

The existing `_query.ts` template changes to use module entity adapters for module operations:

```mustache
{{={= =}=}}
import { prisma } from 'wasp/server'
{=& jsFn.importStatement =}

export default async function (args, context) {
  return ({= jsFn.importIdentifier =} as any)(args, {
    ...context,
    entities: {
      {=# isModuleOperation =}
      {=# moduleEntityMappings =}
      {= alias =}: prisma.{= realPrismaIdentifier =},
      {=/ moduleEntityMappings =}
      {=/ isModuleOperation =}
      {=^ isModuleOperation =}
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
      {=/ isModuleOperation =}
    },
  })
}
```

For module operations, the entity map uses `alias → realPrismaIdentifier` instead of `name → prismaIdentifier`. The module's code accesses `context.entities.Todo` and gets `prisma.todoItem` at runtime.

#### Client module operations (new template)

```mustache
{{={= =}=}}
import { type QueryFor, createQuery } from '../queries/core'
import { type ActionFor, createAction } from '../actions/core'
{=# moduleQueries =}
import type { {= serverFnImportIdentifier =} } from '{=& serverFnImportPath =}'
{=/ moduleQueries =}
{=# moduleActions =}
import type { {= serverFnImportIdentifier =} } from '{=& serverFnImportPath =}'
{=/ moduleActions =}

{=# moduleQueries =}
export const {= operationName =}: QueryFor<typeof {= serverFnImportIdentifier =}> =
  createQuery<typeof {= serverFnImportIdentifier =}>(
    '{= queryRoute =}',
    {=& entitiesArray =},
  )
{=/ moduleQueries =}

{=# moduleActions =}
export const {= operationName =}: ActionFor<typeof {= serverFnImportIdentifier =}> =
  createAction<typeof {= serverFnImportIdentifier =}>(
    '{= actionRoute =}',
    {=& entitiesArray =},
  )
{=/ moduleActions =}
```

### 3.7 Compiler changes (Haskell, detailed)

#### New AppSpec types

```haskell
-- In AppSpec/Module.hs (new file)
data EntityRequirement = EntityRequirement
  { erAlias :: String
  , erRequiredFields :: Maybe (Map String PrismaFieldType)  -- Nothing = alias-only
  }

data ModuleRequirements = ModuleRequirements
  { mrEntities :: [EntityRequirement]
  , mrRequiresAuth :: Bool
  }

-- In TsAppSpec (TypeScript-side representation)
-- Added to TsModuleSpec:
--   entityRequirements?: Map<string, EntityRequirementConfig>
--   requiresAuth?: boolean

-- New second argument to App.use():
data ModuleUseConfig = ModuleUseConfig
  { entityMap :: Map String String  -- alias → real entity name
  }
```

#### Modifications to `App.use()`

```typescript
// App.ts — modified signature
use(module: Module, config?: { entityMap?: Record<string, string> }): void {
  // ... existing declaration merging and path rewriting ...

  // NEW: Store entity map for this module
  if (config?.entityMap) {
    this.#moduleEntityMaps.push({
      packageName: original.packageName!,
      entityMap: config.entityMap,
    });
  }

  // NEW: For module operations, replace entity string names with mapped real names
  // "Todo" in module → "TodoItem" in host (via entityMap)
}
```

#### New validation phase (Haskell)

Between AppSpec analysis and code generation:

```haskell
validateModules :: AppSpec -> [ModuleUse] -> Either [CompileError] ()
validateModules spec modules = do
  forM_ modules $ \mod -> do
    -- 1. Validate each entity mapping exists
    forM_ (entityMap mod) $ \(alias, realName) ->
      unless (entityExists spec realName) $
        throwError $ EntityNotFoundError (modulePackage mod) alias realName

    -- 2. If field-level requirements exist, validate structural compatibility
    forM_ (entityRequirements mod) $ \req ->
      case Map.lookup (erAlias req) (entityMap mod) of
        Nothing -> throwError $ UnmappedEntityError (modulePackage mod) (erAlias req)
        Just realName -> validateFields spec realName (erRequiredFields req)

    -- 3. Validate auth requirement
    when (requiresAuth mod && not (hasAuth spec)) $
      throwError $ AuthRequiredError (modulePackage mod)
```

#### Generator modifications

In `SdkGenerator.hs`, add generation of per-module client operations file:
```haskell
genModuleClientOperations :: AppSpec -> [ModuleUse] -> Generator [FileDraft]
genModuleClientOperations spec modules = do
  forM modules $ \mod -> do
    let queries = getModuleQueries spec mod
    let actions = getModuleActions spec mod
    let entityMap = muEntityMap mod
    -- For each module operation, resolve entity aliases to real entity names
    -- Generate using new module client operations template
    mkTmplFdWithData moduleClientOpsTmpl (buildTemplateData queries actions entityMap)
```

In `ServerGenerator/OperationsG.hs`, modify `operationTmplData` to include module entity mappings:
```haskell
operationTmplData :: Maybe ModuleUse -> AS.Operation.Operation -> Aeson.Value
operationTmplData maybeModuleUse operation =
  object $
    [ "jsFn" .= ...,
      "isModuleOperation" .= isJust maybeModuleUse
    ] ++ case maybeModuleUse of
      Just mod ->
        [ "moduleEntityMappings" .= map (makeModuleEntityMapping (muEntityMap mod))
            (fromMaybe [] $ AS.Operation.getEntities operation)
        ]
      Nothing ->
        [ "entities" .= ... ]  -- existing app entity handling
```

---

## Part 4: Developer Experience

### Module authoring workflow

```
1. Create module
   $ mkdir @myorg/payments-module && cd $_
   $ npm init
   $ npm install --save-dev wasp-config typescript

2. Write module factory (src/index.ts)
   - mod.entity("Subscription")
   - mod.requiresAuth()
   - mod.query("getSubscription", { ... })
   - mod.action("createCheckout", { ... })

3. Write server implementations (src/queries.ts, src/actions.ts)
   - import { AuthenticatedQuery } from "wasp-config/module"
   - Use typed context.entities.Subscription

4. Write client components (src/SubscriptionPage.tsx)
   - import { getSubscription } from "#module/operations"
   - import { useQuery } from "wasp/client/operations"
   - useQuery(getSubscription) → fully typed

5. Write stubs for standalone dev (src/_stubs/operations.ts)
   - Re-export from wasp/client/operations (available in dev app)

6. Test
   $ npx tsc --noEmit       # type check
   $ npx vitest             # unit tests with mock PrismaDelegate

7. Build & publish
   $ npx tsc                # compile to dist/
   $ npm publish
```

### Module testing with mocks

```typescript
// tests/queries.test.ts
import { getTodos } from '../src/queries';

function createMockDelegate<T>(data: T[]): PrismaDelegate<T> {
  return {
    findMany: async () => data,
    findUnique: async ({ where }) => data.find(d => d.id === where.id) ?? null,
    create: async ({ data: newItem }) => ({ ...newItem, id: data.length + 1 }) as T,
    // ... etc
  };
}

test('getTodos returns all todos for user', async () => {
  const todos = [{ id: 1, text: 'test', isDone: false, userId: 42 }];
  const context = {
    entities: { Todo: createMockDelegate(todos) },
    user: { id: 42, identities: {}, getFirstProviderUserId: () => null },
  };

  const result = await getTodos(undefined, context);
  expect(result).toEqual(todos);
});
```

The `wasp-config/module` package could ship a `createMockDelegate` helper to make this even easier.

### Module package structure

```
@myorg/payments-module/
├── src/
│   ├── index.ts              # Module factory + interface declarations
│   ├── queries.ts            # Server query implementations
│   ├── actions.ts            # Server action implementations
│   ├── SubscriptionPage.tsx  # Client component
│   ├── _stubs/
│   │   └── operations.ts     # Stubs for #module/operations in dev
│   └── types.ts              # Module's entity TS types
├── dist/                     # tsc output
├── package.json              # peerDependencies: wasp-config
├── tsconfig.json
└── tests/
    ├── queries.test.ts
    └── actions.test.ts
```

### Host app consumption

```typescript
// waspello/main.wasp.ts
import { createPaymentsModule } from "@myorg/payments-module";

const app = new App("waspello", { ... });

app.use(createPaymentsModule({
  premiumPlanPriceId: "price_...",
  subscriptionRoute: "/subscription",
}), {
  entityMap: {
    Subscription: "UserSubscription",  // module's "Subscription" → host's entity
    User: "AppUser",                   // module's "User" → host's entity
  },
});

export default app;
```

---

## Part 5: Comparison — Current vs Proposed

| Aspect | Current FSM | Proposed |
|---|---|---|
| Entity reference | `context.entities[todoEntityName]` (string, untyped) | `context.entities.Todo` (typed property) |
| Entity validation | None (runtime error) | Compiler validates (Approach A) or TS structural typing (Approach B) |
| Module SDK dependency | `wasp/server/module` (app-generated) | `wasp-config/module` (published npm package) |
| Dev experience | Need dev app + schema.prisma + Wasp compiler | Pure TypeScript: `tsc` + `vitest` |
| Type safety | `PrismaDelegate<any>`, unsafe casts | `PrismaDelegate<Todo>`, fully typed |
| Entity mapping | `provide("todoEntityName", "TodoItem")` | `entityMap: { Todo: "TodoItem" }` in `app.use()` |
| Client operations | Import from `wasp/client/operations` (app-generated) | Import from `#module/operations` (package-scoped, compiler-resolved) |
| Auth access | `AuthUser` from generated SDK | `AuthUser` from `wasp-config/module` (generic) |
| HttpError | From `wasp/server` (app-generated) | From `wasp-config/module` (standalone) |

---

## Part 6: Risks & Open Questions

### Risks

1. **Prisma type compatibility**: Structural validation must handle Prisma modifiers (`@default`, `@id`, `@unique`, `@relation`). A field `isDone Boolean @default(false)` should match requirement `isDone: "Boolean"`. Mitigation: validate base type only, ignore modifiers.

2. **TypeScript type drift**: Module author's TS types could diverge from their `mod.entity()` declarations. Mitigation: Ship a codegen tool that generates TS types from entity declarations, or validate at compile time.

3. **`#module/` path rewriting complexity**: The compiler needs to intercept `#module/` imports in compiled module JS and rewrite them. This is a new rewriting dimension beyond `@src/` → `@pkg/`. Mitigation: Consider using `@module/` prefix instead (extends existing `@` prefix handling).

4. **Peer dependency version conflicts**: Module depends on `wasp-config` as peerDep, host app has a different version. Standard npm problem, mitigated by semver ranges.

### Open questions

1. **Relations**: How does a module express "Todo has a foreign key to User"? Options: explicit `relations` in `mod.entity()`, or implicit via field names.

2. **`provide()` future**: With `entityMap` handling entity wiring, `provide()` would only be used for non-entity runtime config. Could be simplified or removed.

3. **`wasp-config/module` vs new package**: Adding module types to existing `wasp-config` is simpler. Creating `wasp-module-sdk` is cleaner separation. Recommend starting with `wasp-config/module` export.

4. **Module-to-module dependencies**: Can one module depend on another module's entities/operations? Not in v1, but the architecture supports it via chained entity maps.

---

## Key files to modify

| File | Change |
|---|---|
| `waspc/data/packages/wasp-config/src/publicApi/Module.ts` | Add `entity()`, `requiresAuth()` methods |
| `waspc/data/packages/wasp-config/src/publicApi/App.ts` | Extend `use()` to accept `entityMap`, store module entity maps |
| `waspc/data/packages/wasp-config/src/publicApi/tsAppSpec.ts` | Add `entityRequirements`, `requiresAuth`, `entityMap` to `TsModuleSpec` |
| `waspc/data/Generator/templates/sdk/wasp/server/module/index.ts` | Make operation types generic with `Entities` type parameter |
| `waspc/data/Generator/templates/server/src/queries/_query.ts` | Add module entity mapping branch |
| `waspc/data/Generator/templates/server/src/actions/_action.ts` | Same |
| `waspc/src/Wasp/Generator/ServerGenerator/OperationsG.hs` | Extend `operationTmplData` for module operations |
| `waspc/src/Wasp/Generator/SdkGenerator/Client/OperationsGenerator.hs` | Generate per-module client operations file |
| `waspc/src/Wasp/Generator/SdkGenerator.hs` | Add module adapter generation |
| New: `waspc/src/Wasp/AppSpec/Module.hs` | Define `EntityRequirement`, `ModuleRequirements` types |
| New: `waspc/src/Wasp/Generator/ModuleValidator.hs` | Entity mapping validation logic |
| New: module client operations template | `createQuery`/`createAction` for module operations |
