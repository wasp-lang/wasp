# @waspello/todo-module — Context

## What this is

A proof-of-concept **Wasp module** — a reusable, distributable npm package that
registers Wasp declarations (pages, queries, actions, APIs, CRUD, jobs, setup
hooks) into a host app via `app.use(createTodoModule({ ... }))`.

## Key concept: `Module` from `wasp-config`

`createTodoModule` (in `src/index.ts`) instantiates `Module` and calls its
builder methods (`mod.page`, `mod.query`, `mod.action`, `mod.api`,
`mod.apiNamespace`, `mod.crud`, `mod.job`, `mod.serverSetup`,
`mod.clientSetup`). These mirror the methods on `App` but scope declarations to
the module's package.

The module is entity-agnostic: the host app passes `entityName` (e.g.
`"TodoItem"`) and the module uses it everywhere via `mod.provide("entityName",
...)` + the generated virtual import `"wasp/module/@waspello/todo-module"`.

## File overview

| File | Purpose |
|---|---|
| `index.ts` | Module entry — builds all declarations |
| `queries.ts` | `getTodos` query |
| `actions.ts` | `createTodo`, `updateTodo`, `deleteTodo` (zod-validated) |
| `apis.ts` | Express handlers: `GET /api/todos`, `GET /api/todos/stats` |
| `crud.ts` | CRUD `getAll` override (filters out done todos) |
| `jobs.ts` | `cleanDoneTodos` PgBoss job (deletes done items on cron) |
| `middleware.ts` | `todoApiMiddleware` for the `/api/todos` namespace (no-op) |
| `clientSetup.ts` | Client init hook (log-only) |
| `serverSetup.ts` | Server init hook (log-only) |
| `TodoPage.tsx` | React page — uses `useQuery`, actions, CRUD hooks, and `api` |
| `store.ts` | Shared `Todo` type |
| `sdk.d.ts` | Ambient type stubs for `wasp/*` imports + `OperationContext` |

## How the host app uses it

In `main.wasp.ts`:

```ts
app.use(
  createTodoModule({
    entityName: "TodoItem",
    route: "/todos",
    cleanDoneTodosCron: "* * * * *",
  }),
);
```

## Entity access pattern

All server-side files import `entityName` from the virtual module and use
`context.entities[entityName]` for dynamic Prisma delegate access.

## Local dev

- `npm run build` — compiles TS to `dist/`
- `npm run local-registry` — publishes to a local Verdaccio for integration testing
- Currently linked via `file:` in waspello's `package.json`
