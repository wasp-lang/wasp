# Wasp Full-Stack Modules (FSM)

## What is it?

FSM is a module system for Wasp that lets third-party npm packages declaratively add full-stack features — pages, routes, queries, actions, APIs, jobs, middleware, and setup hooks — to a Wasp app. A module author writes a factory function that returns a `Module` describing all its declarations. The host app consumes it with `app.use()`.

## How a module looks

A module is a standard npm package that depends on `wasp-config` (dev) and exports a factory function:

```typescript
// @waspello/todo-module/src/index.ts
import { Module } from "wasp-config";

export function createTodoModule(config: TodoModuleConfig): Module {
  const mod = new Module("@waspello/todo-module");

  mod.provide("todoEntityName", config.todoEntityName);

  mod.page("Todos", { component: { importDefault: "TodoPage", from: "@src/TodoPage" } });
  mod.route("TodosRoute", { path: config.route, to: todosPage });
  mod.query("getTodos", { fn: { import: "getTodos", from: "@src/queries" }, entities: [...], auth: true });
  mod.action("createTodo", { fn: { ... }, entities: [...] });
  mod.job("cleanDoneTodos", { executor: "PgBoss", perform: { fn: { ... } }, schedule: { cron: "..." } });
  mod.serverSetup({ import: "initServer", from: "@src/serverSetup" });
  mod.clientSetup({ import: "initClient", from: "@src/clientSetup" });

  return mod;
}
```

The `Module` class supports: `page`, `route`, `query`, `action`, `api`, `apiNamespace`, `crud`, `job`, `serverSetup`, `clientSetup`, and `provide` (for runtime config).

## How the host app consumes modules

```typescript
// waspello/main.wasp.ts
import { createStripePaymentsModule } from "@waspello/stripe-payments";
import { createTodoModule } from "@waspello/todo-module";

const app = new App("waspello", { title: "Waspello", wasp: { version: "^0.21.2" } });

app.use(createStripePaymentsModule({
  userEntityName: "User",
  premiumPlanPriceId: "price_...",
  subscriptionRoute: "/subscription",
}));

app.use(createTodoModule({
  todoEntityName: "TodoItem",
  route: "/todos",
  userEntityName: "User",
  userForeignKey: "userId",
}));

export default app;
```

`app.use()` merges the module's declarations into the app's declaration list, rewrites `@src/` import paths to `@pkg/@waspello/todo-module/...` (so they resolve to the npm package's exports), and collects setup functions and `provide()` values.

## Dev vs shipped mode

The same module can run standalone for development or be consumed by a host app:

| Scenario | What happens |
|---|---|
| **Dev** — module has its own `main.wasp.ts` | `@src/` paths stay as-is, resolved against module's own `src/` |
| **Shipped** — host app installs the module | `@src/` paths are rewritten to `@pkg/packageName/...`, resolved via npm package exports |

This is determined by comparing `packageName` on the `Module` with the host app's `projectPackageName` from its `package.json`.

## What the compiler does

The Wasp compiler (Haskell) receives the flattened declarations from all modules and the host app. Key codegen details:

- **Server setup** (`waspc/src/Wasp/Generator/ServerGenerator.hs`): Imports each module's `serverSetup` function with indexed aliases (`moduleSetupFn_0`, `moduleSetupFn_1`) to avoid name collisions, then calls them sequentially during startup.
- **Client setup** (`waspc/src/Wasp/Generator/SdkGenerator/Client/VitePlugin/VirtualModulesPluginG.hs`): Same pattern — indexed aliases (`moduleClientSetupFn_0`, etc.) in the Vite virtual entry point.
- **Runtime config** (`waspc/data/Generator/templates/sdk/wasp/modules/config.ts`): The compiler generates a `Map<string, Record<string, unknown>>` from all `provide()` calls. Module code reads it at runtime: `config.get(PACKAGE_NAME)`.
- **Module types** (`waspc/data/Generator/templates/sdk/wasp/server/module/index.ts`): Provides `AuthenticatedQueryDefinition`, `AuthenticatedActionDefinition`, etc. — simplified versions of the SDK's operation types that use `Record<string, PrismaDelegate>` instead of typed entities (since modules don't know the host's entities at compile time).

## Build and distribution

Modules are built with `tsc`, packed with `npm pack`, and installed in the host app as tarballs (not symlinks). This is critical — tarball installs place the package inside `node_modules/` so Node can resolve `wasp-config` and other peer deps from the host app.

The `build-and-install.sh` script automates: `lw clean` + `lw compile` (install module dev deps) → `npm run build` → `npm pack` → bust lockfile cache → `npm install --force` in waspello.

## Two prototype modules

- **`@waspello/todo-module`**: Adds a todo list page with CRUD operations, a cron job to clean done todos, and a REST API endpoint. Demonstrates queries, actions, jobs, CRUD, API, middleware, auth.
- **`@waspello/stripe-payments`**: Adds a subscription page, Stripe checkout flow, webhook handling, and subscription status queries. Demonstrates pages, actions, APIs, middleware namespaces, server/client setup hooks, and runtime config.

## Key architectural decisions

1. **Flat namespace** — All declarations from all modules share one global namespace. Duplicates are caught at the TypeScript layer with error messages showing provenance.
2. **No entity declarations** — Modules cannot define Prisma models. The host app declares entities; modules reference them by name string via `provide()`.
3. **Untyped runtime config** — `provide()` values are JSON-serialized and accessed via `config.get()` with an unsafe cast. Type safety is the module author's responsibility.
4. **Symbol-based private state** — Module specs are stored on objects using `Symbol.for("wasp:module-spec")` so they survive multiple copies of `wasp-config` in the dependency tree.

## Known limitations / open issues

- **No schema merging** — Modules can't contribute Prisma schema; host must declare all entities.
- **No env var support in config** — `provide()` values are evaluated at compile time when `main.wasp.ts` runs, so `process.env` reads happen before `.env.server` is loaded.
- **Tarball cache busting** — npm caches tarballs by version, so reinstalling a changed module at the same version requires stripping the integrity hash from `package-lock.json` and force-installing.
- **No LSP awareness** — The language server doesn't know about modules.
- **TypeScript-only** — The `.wasp` DSL doesn't support modules, only `main.wasp.ts`.
