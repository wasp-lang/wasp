# Wasp Full-Stack Modules (FSM)

## What is it?

FSM is a module system for Wasp that lets third-party npm packages declaratively add full-stack features — pages, routes, queries, actions, APIs, jobs, middleware, and setup hooks — to a Wasp app. A module author writes a factory function that returns a `Module` describing all its declarations. The host app consumes it with `app.use()`.

## How a module looks

A module is a standard npm package that depends on `wasp-config` (dev) and exports a factory function from `src/index.ts`. It also has a `module.wasp.ts` entry point that calls the factory and exports the result:

```typescript
// @waspello/todo-module/src/index.ts
import { Module } from "wasp-config";

export function createTodoModule(config: TodoModuleConfig): Module {
  const mod = new Module("@waspello/todo-module");

  // Declare entity requirements with Prisma field types
  mod.entity("Todo", {
    fields: {
      id: "Int @id @default(autoincrement())",
      text: "String",
      isDone: "Boolean @default(false)",
    },
  });
  mod.requiresAuth();

  mod.provide("userForeignKey", config.userForeignKey);

  mod.page("Todos", { component: { importDefault: "TodoPage", from: "@src/TodoPage" }, authRequired: true });
  mod.route("TodosRoute", { path: config.route, to: todosPage });
  mod.query("getTodos", { fn: { import: "getTodos", from: "@src/queries" }, entities: ["Todo"], auth: true });
  mod.action("createTodo", { fn: { ... }, entities: ["Todo"] });
  mod.job("cleanDoneTodosJob", { executor: "PgBoss", perform: { fn: { ... } }, schedule: { cron: "..." } });
  mod.serverSetup({ import: "initServer", from: "@src/serverSetup" });
  mod.clientSetup({ import: "initClient", from: "@src/clientSetup" });

  return mod;
}
```

```typescript
// @waspello/todo-module/module.wasp.ts
import { createTodoModule } from "./src/index.js";

export default createTodoModule({ route: "/todos" });
```

The `Module` class supports: `entity`, `requiresAuth`, `page`, `route`, `query`, `action`, `api`, `apiNamespace`, `crud`, `job`, `serverSetup`, `clientSetup`, and `provide` (for runtime config).

## How the host app consumes modules

```typescript
// waspello/main.wasp.ts
import { createStripePaymentsModule } from "@waspello/stripe-payments";
import { createTodoModule } from "@waspello/todo-module";

const app = new App("waspello", { title: "Waspello", wasp: { version: "^0.21.2" } });

app.use(
  createStripePaymentsModule({
    premiumPlanPriceId: "price_...",
    subscriptionRoute: "/subscription",
  }),
  {
    entityMap: {
      User: "User",
    },
  },
);

app.use(
  createTodoModule({
    route: "/todos",
    useAuth: true,
    userForeignKey: "userId",
  }),
  {
    entityMap: {
      Todo: "TodoItem",
      User: "User",
    },
  },
);

export default app;
```

`app.use()` takes two arguments: the module instance and a config with `entityMap`. It merges the module's declarations into the app, rewrites `@src/` import paths to `@pkg/@waspello/todo-module/...` (so they resolve to the npm package's exports), and maps module entity aliases to host entity names via `entityMap`.

## Entity declarations and validation

Modules declare their entity requirements using `mod.entity()` with Prisma field definitions:

```typescript
mod.entity("User", {
  fields: {
    id: "Int @id @default(autoincrement())",
    email: "String?",
    stripeCustomerId: "String?",
  },
});
```

The compiler validates that host entities (mapped via `entityMap`) have compatible fields. The `mod.requiresAuth()` method declares that the module requires the host app to have auth configured.

## Dev vs shipped mode

The same module can run standalone for development or be consumed by a host app:

| Scenario | What happens |
|---|---|
| **Dev** — module has its own `module.wasp.ts` | `@src/` paths stay as-is, resolved against module's own `src/` |
| **Shipped** — host app installs the module | `@src/` paths are rewritten to `@pkg/packageName/...`, resolved via npm package exports |

Each module has a `module.wasp.ts` that calls the factory with dev defaults — this is the entry point for standalone development with `wasp module build`.

## What the compiler does

The Wasp compiler (Haskell) receives the flattened declarations from all modules and the host app. Key codegen details:

- **Server setup** (`waspc/src/Wasp/Generator/ServerGenerator.hs`): Imports each module's `serverSetup` function with indexed aliases (`moduleSetupFn_0`, `moduleSetupFn_1`) to avoid name collisions, then calls them sequentially during startup.
- **Client setup** (`waspc/src/Wasp/Generator/SdkGenerator/Client/VitePlugin/VirtualModulesPluginG.hs`): Same pattern — indexed aliases (`moduleClientSetupFn_0`, etc.) in the Vite virtual entry point.
- **Runtime config** (`waspc/data/Generator/templates/sdk/wasp/modules/config.ts`): The compiler generates a `Map<string, Record<string, unknown>>` from all `provide()` calls. Module code reads it at runtime: `config.get(PACKAGE_NAME)`.
- **Module types** (`waspc/data/Generator/templates/sdk/wasp/server/module/index.ts`): Provides `AuthenticatedQueryDefinition`, `AuthenticatedActionDefinition`, etc. — simplified versions of the SDK's operation types that use `Record<string, PrismaDelegate>` instead of typed entities (since modules don't know the host's entities at compile time).
- **Entity validation**: The compiler validates that each mapped host entity has the fields required by the module's `mod.entity()` declarations. Also validates `requiresAuth` against the host app's auth config.

## Build and distribution

Modules are built with `wasp module build` (generates `.wasp/` artifacts including `decls.json` and a local SDK workspace) followed by `tsc -p tsconfig.build.json`. They're packed with `npm pack` and installed in the host app as tarballs (not symlinks). Tarball installs place the package inside `node_modules/` so Node can resolve `wasp-config` and other peer deps from the host app.

The `build-and-install.sh` script automates: `npm install` → `wasp module build` + `tsc` → `npm pack` → bust lockfile cache → `npm install --force` in waspello.

## Two prototype modules

- **`@waspello/todo-module`**: Adds a todo list page with CRUD operations, a cron job to clean done todos, and REST API endpoints. Demonstrates queries, actions, jobs, CRUD, API, middleware, auth. Conditionally requires auth via `useAuth` config flag.
- **`@waspello/stripe-payments`**: Adds a subscription page, Stripe checkout flow, webhook handling, and subscription status queries. Demonstrates pages, actions, APIs, middleware namespaces, server/client setup hooks, runtime config, and entity field validation. Always requires auth.

## Key architectural decisions

1. **Flat namespace** — All declarations from all modules share one global namespace. Duplicates are caught at the TypeScript layer with error messages showing provenance.
2. **Entity declarations with field validation** — Modules declare entity requirements via `mod.entity("Name", { fields })` with Prisma field syntax. The compiler validates host entities have compatible fields. Host maps module aliases to real entities via `entityMap` in `app.use()`.
3. **Untyped runtime config** — `provide()` values are JSON-serialized and accessed via `config.get()` with an unsafe cast. Type safety is the module author's responsibility.
4. **Symbol-based private state** — Module specs are stored on objects using `Symbol.for("wasp:module-spec")` so they survive multiple copies of `wasp-config` in the dependency tree.
5. **`module.wasp.ts` entry point** — Each module has a dedicated `module.wasp.ts` that calls the factory with dev defaults, serving as the entry point for `wasp module build`.

## Known limitations / open issues

- **No schema merging** — Modules can't contribute Prisma schema; host must declare all entities.
- **No env var support in config** — `provide()` values are evaluated at compile time when `main.wasp.ts` runs, so `process.env` reads happen before `.env.server` is loaded.
- **Tarball cache busting** — npm caches tarballs by version, so reinstalling a changed module at the same version requires stripping the integrity hash from `package-lock.json` and force-installing.
- **No LSP awareness** — The language server doesn't know about modules.
- **TypeScript-only** — The `.wasp` DSL doesn't support modules, only `main.wasp.ts`.

## Design docs

- **ISOLATION.md** — Research and design document for interface-based module isolation: typed entity access (`context.entities.Todo` instead of `context.entities[todoEntityName]`), `#module/operations` client imports, and compiler-generated entity adapters.
- **steps/** — Step-by-step implementation plan (01 through 08) covering wasp-config foundation, module types, Haskell IR, CLI commands, build pipeline, SDK generation, host entity validation, and scaffolding.
