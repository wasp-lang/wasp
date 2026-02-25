# Wasp Full Stack Modules (FSM) — Analysis

## 1. How Modules Are Authored

### Structure

A module is a standard npm package. The canonical example is `examples/waspello/packages/todo-module/`.

```
package.json              # npm package, e.g. "@waspello/todo-module"
src/
  index.ts                # exports factory function: createTodoModule(config) → Module
  queries.ts              # server-side query implementations
  actions.ts              # server-side action implementations
  apis.ts                 # Express API handlers
  crud.ts                 # CRUD getAll override
  jobs.ts                 # cleanDoneTodos PgBoss job
  middleware.ts            # todoApiMiddleware for /api/todos namespace
  auth.ts                 # auth helpers
  config.ts               # reads runtime config from wasp/modules/config
  serverSetup.ts          # server initialization hook
  clientSetup.ts          # client initialization hook
  TodoPage.tsx            # React page component
  store.ts                # shared Todo type
  vite-env.d.ts           # Vite client type reference
  scaffold/
    LoginPage.tsx         # login page (standalone dev only)
    SignupPage.tsx        # signup page (standalone dev only)
main.wasp.ts              # standalone dev app (for developing the module in isolation)
schema.prisma             # Prisma schema (used only for standalone dev)
tsconfig.build.json       # compiles src/ → dist/
tsconfig.wasp.json        # for main.wasp.ts analysis in standalone dev
```

### Authoring API

The module author imports `Module` from `wasp-config` and calls builder methods:

```ts
import { Module } from "wasp-config";

export function createTodoModule(config: TodoModuleConfig): Module {
  const mod = new Module("@waspello/todo-module");

  const todosPage = mod.page("Todos", { component: { importDefault: "TodoPage", from: "@src/TodoPage" } });
  mod.route("TodosRoute", { path: config.route, to: todosPage });
  mod.query("getTodos", { fn: { import: "getTodos", from: "@src/queries" }, entities, auth: true });
  mod.action("createTodo", { fn: { import: "createTodo", from: "@src/actions" }, entities });
  mod.serverSetup({ import: "initServer", from: "@src/serverSetup" });
  mod.clientSetup({ import: "initClient", from: "@src/clientSetup" });
  mod.provide("todoEntityName", config.todoEntityName);

  return mod;
}
```

### Class Hierarchy

`Module.ts` defines two classes:

- **`AppDeclBuilder`** (internal) — accepts an optional `packageName`. Used by `App` internally to collect the app's own declarations.
- **`Module`** (public, extends `AppDeclBuilder`) — requires `packageName`. This is what module authors use.

### Capabilities

The `Module` class (`data/packages/wasp-config/src/publicApi/Module.ts`) exposes:

| Method | Registers |
|---|---|
| `page(name, config)` | React page component (returns `PageName` token) |
| `route(name, config)` | Client-side route (references a `PageName`) |
| `query(name, config)` | Server query (fn + entities + auth) |
| `action(name, config)` | Server action (fn + entities + auth) |
| `api(name, config)` | Express API endpoint (fn + httpRoute + entities + auth + middleware) |
| `apiNamespace(name, config)` | API namespace for middleware grouping |
| `crud(name, config)` | CRUD group over an entity |
| `job(name, config)` | Background job (PgBoss executor) |
| `serverSetup(fn)` | Server initialization hook |
| `clientSetup(fn)` | Client initialization hook |
| `provide(key, value)` | Publishes a JSON-serializable value accessible at runtime |

### Limitations

1. **No entity declarations.** Modules cannot define Prisma entities — the host app must declare them in `schema.prisma`. The module accepts entity names as config parameters and accesses them dynamically via `context.entities[entityName]`.

2. **No schema merging.** There is no mechanism to merge a module's `schema.prisma` into the host.

3. **`provide()` values are untyped at runtime.** The Haskell side stores them as `Map String Value` (Aeson). Module runtime code does an unsafe cast: `config.get(PACKAGE_NAME)! as RuntimeConfig`.

4. **`ResolveExtImports` does not recurse into arrays.** If a future module-level config uses `ExtImport[]`, the type would need an array branch.

5. **No LSP support.** The Wasp language server knows nothing about modules — no autocomplete or goto-definition for module-contributed declarations.

6. **`AppDeclBuilder` without `packageName` cannot use `provide()`.** The internal `AppDeclBuilder` class throws when `provide()` is called without a `packageName`. The public `Module` class always requires `packageName`, so this only affects internal usage.

7. **`.wasp` DSL cannot use modules.** `ModuleProvide` deliberately errors from the `.wasp` analyzer — modules are TypeScript-config-only (`main.wasp.ts`).

---

## 2. How Modules Are Tested in Development

### Test Infrastructure

The `wasp-config` package (`data/packages/wasp-config/`) uses Vitest with three test projects:

```ts
// vitest.workspace.ts
defineWorkspace([
  { test: { name: "unit",        include: ["**/__tests__/**/*.unit.test.ts"] } },
  { test: { name: "integration", include: ["**/__tests__/**/*.integration.test.ts"] } },
  { test: { name: "type",        include: ["**/__tests__/**/*.test-d.ts"],
             typecheck: { enabled: true, only: true } } },
])
```

### Test Coverage

| Test file | What it covers |
|---|---|
| `__tests__/module.unit.test.ts` | `Module` and `AppDeclBuilder` declaration collection, `App.use()` merging, duplicate detection with provenance error messages, path rewriting |
| `__tests__/mapTsAppSpecToAppSpecDecls.unit.test.ts` | Every `map*` function with minimal/full fixtures, error paths, module-specific fields |
| `__tests__/mapTsAppSpecToAppSpecDecls.integration.test.ts` | Full `App` → `Decl[]` end-to-end |
| `__tests__/appAnalyzer.unit.test.ts` | `analyzeApp` pipeline via `vi.doMock()` — success and error paths |
| `__tests__/cli.unit.test.ts` | `parseProcessArgsOrThrow` argument validation |
| `__tests__/testFixtures.test-d.ts` | Type-level tests for `MinimalConfig<T>` / `FullConfig<T>` |
| `__tests__/testFixtures.ts` | Fixture factory: `createApp("minimal"\|"full")`, per-config-type fixtures |

### Standalone Module Development

A module can include its own `main.wasp.ts` for isolated development:

```ts
// packages/todo-module/main.wasp.ts
import { App } from "wasp-config";
import { createTodoModule } from "./src/index.js";

const app = new App("todoModuleDev", {
  title: "Todo Module Dev",
  wasp: { version: "^0.21.2" },
});

app.auth({
  userEntity: "User",
  methods: { usernameAndPassword: {} },
  onAuthFailedRedirectTo: "/login",
});

app.use(
  createTodoModule({
    todoEntityName: "TodoItem",
    route: "/",
    userEntityName: "User",
    userForeignKey: "userId",
  }),
);

export default app;
```

Running `wasp start` inside the module directory compiles and runs the standalone app. This works because of the **dev vs shipped** distinction (see below).

### Dev vs Shipped Module

The distinction is controlled by `App.projectPackageName` — set from the host project's `package.json` name during `wasp-config` CLI execution.

**In `App.use()` (`data/packages/wasp-config/src/publicApi/App.ts`):**

```ts
if (packageName && packageName !== App.projectPackageName) {
  App.#rewriteModuleImports(incoming, packageName);
}
```

| Scenario | `packageName` vs `projectPackageName` | Import paths | Effect |
|---|---|---|---|
| **Dev mode** (module's own `main.wasp.ts`) | Same | `@src/foo` stays as `@src/foo` | Module resolves imports from its own `src/` |
| **Shipped** (host app uses installed module) | Different | `@src/foo` → `@pkg/@waspello/todo-module/foo` | Host app resolves imports from the npm package |

The `@pkg/` prefix maps to `ExtImportPkgPath` in Haskell, which is emitted verbatim as a bare package import in generated code (e.g., `import { getTodos } from '@waspello/todo-module/queries'`). Node.js resolves this via the module's `package.json` exports.

### Local Registry for Integration Testing

The todo-module ships a Verdaccio-based local registry script (`packages/todo-module/scripts/local-registry.mjs`) that:

1. Starts a Verdaccio instance at `http://localhost:4873`
2. Publishes `wasp-config` from the monorepo to the local registry
3. Publishes the todo-module to the local registry
4. The host app can then `npm install @waspello/todo-module@1.0.0 --registry http://localhost:4873`

---

## 3. How Modules Are Shipped, Installed, and Configured

### Compilation Pipeline

```
main.wasp.ts
    │  tsc compiles to .wasp/main.wasp.js
    ▼
npx wasp-config .wasp/main.wasp.js .wasp/decls.json [entityNames]
    │  dynamically imports compiled JS
    │  calls app[GET_TS_APP_SPEC]() → TsAppSpec
    │  maps via mapTsAppSpecToAppSpecDecls() → Decl[]
    │  writes JSON
    ▼
.wasp/decls.json
    │  Haskell deserializes via FromJSON
    ▼
[AS.Decl]  →  standard generator pipeline
```

Key files:
- `src/Wasp/Project/WaspFile/TypeScript.hs` — Haskell driver
- `data/packages/wasp-config/src/run.ts` — CLI entry point
- `data/packages/wasp-config/src/appAnalyzer.ts` — analysis entry
- `data/packages/wasp-config/src/mapTsAppSpecToAppSpecDecls.ts` — mapping layer

### `wasp-config` Package Installation

The `wasp-config` package ships inside the `waspc` binary. On first encounter of a `.wasp.ts` file (`src/Wasp/Project/WaspFile/WaspConfigPackage.hs`):

1. Copies `wasp-config` source from Wasp's data directory to `.wasp/packages/wasp-config/`
2. Runs `npm install --save-dev file:.wasp/packages/wasp-config`

### Installation from the Host App

```sh
npm install @waspello/todo-module
```

Then in `main.wasp.ts`:

```ts
import { createTodoModule } from "@waspello/todo-module";
app.use(createTodoModule({ todoEntityName: "TodoItem", route: "/todos", ... }));
```

### Internal Data Flow

**TypeScript types** (`data/packages/wasp-config/src/publicApi/tsAppSpec.ts`):

```ts
type TsModuleSpec = {
  actions:      Map<string, ActionConfig>;
  apiNamespaces: Map<string, ApiNamespaceConfig>;
  apis:         Map<string, ApiConfig>;
  cruds:        Map<string, CrudConfig>;
  jobs:         Map<string, JobConfig>;
  pages:        Map<string, PageConfig>;
  queries:      Map<string, QueryConfig>;
  routes:       Map<string, RouteConfig>;
  serverSetupFn?: ExtImport;
  clientSetupFn?: ExtImport;
  provides:     Map<string, JsonSerializable>;
  packageName?: string;
};
```

`TsAppSpec` extends `TsModuleSpec` with app-level singletons (`auth`, `db`, `server`, etc.) plus the aggregated `moduleServerSetupFns[]`, `moduleClientSetupFns[]`, `moduleProvides[]`.

**Haskell types** (`src/Wasp/AppSpec/App.hs`):

```haskell
data App = App { ...
  , moduleServerSetupFns :: Maybe [ExtImport]
  , moduleClientSetupFns :: Maybe [ExtImport]
  , moduleProvides       :: Maybe [ModuleProvide]
  }

data ModuleProvide = ModuleProvide
  { packageName :: String
  , values :: Map.Map String Value
  }
```

Module declarations (pages, routes, queries, etc.) are merged into the flat `[Decl]` list — indistinguishable from host app declarations. Only the three fields above remain module-aware.

### Declaration Provenance

`App` tracks which module contributed each declaration via `#declarationSources: Map<string, string>` (maps `"declType:declName"` → source name). When `App.use()` detects a duplicate, the error message includes both sources:

```
Duplicate queries declaration: 'getData' from '@pkg/b' conflicts with 'getData' from '@pkg/a'.
```

Direct `app.page()`, `app.query()`, etc. calls are recorded with source `"app"`.

### Import Path Resolution

| Stage | Path | Type |
|---|---|---|
| Module author writes | `@src/queries` | Source-relative |
| After `app.use()` rewrite | `@pkg/@waspello/todo-module/queries` | Package-relative |
| Haskell internal | `ExtImportPkgPath "@waspello/todo-module/queries"` | — |
| Generated code | `import { getTodos } from '@waspello/todo-module/queries'` | Node.js resolves via npm |

The module's `package.json` exports (`"./*": "./dist/*.js"`) make subpath imports work.

### Code Generation

Module-specific generated files:

- **Server setup** (`data/Generator/templates/server/src/server.ts`): imports and calls `moduleServerSetupFns` at startup
- **Client setup** (`data/Generator/templates/sdk/wasp/client/vite/virtual-files/files/index.tsx`): imports and calls `moduleClientSetupFns`
- **Runtime config** (`data/Generator/templates/sdk/wasp/modules/config.ts`): generates a `Map<string, Record<string, unknown>>` from `moduleProvides`, accessible via `import { config } from "wasp/modules/config"`
- **Server bundle** (`data/Generator/templates/server/rollup.config.js`): module packages are listed as Rollup externals to avoid bundling symlinked `file:` deps
- **Server module types** (`data/Generator/templates/sdk/wasp/server/module/index.ts`): provides `PrismaDelegate`, `OperationContext`, `AuthUser`, `AuthOperationContext` types for module authors

### Haskell Generator Wiring

- `SdkGenerator.hs`: `genModuleConfigFile` renders `modules/config.ts` with serialized provides
- `ServerGenerator.hs`: `genServerJs` threads `moduleServerSetupFns`; `genRollupConfigJs` threads module package names as externals
- `SdkGenerator/Client/VitePlugin/VirtualModulesPluginG.hs`: `genVirtualIndexTsx` threads `moduleClientSetupFns`
- `Generator/JsImport.hs` + `SdkGenerator/JsImport.hs`: handle `ExtImportPkgPath` as `RawModuleImportPath` (emitted verbatim)

### Private State Mechanism

Module spec is stored via `Symbol.for("wasp:module-spec")` — a global symbol registry key that works across multiple copies of the `wasp-config` package:

```ts
// _private.ts
const MODULE_SPEC_KEY = Symbol.for("wasp:module-spec");

export function getModuleSpec(module: unknown): TsModuleSpec {
  return (module as any)[MODULE_SPEC_KEY];
}

export function setModuleSpec(module: unknown, spec: TsModuleSpec): void {
  (module as any)[MODULE_SPEC_KEY] = spec;
}
```
