// Throughout this file, in order for the constructor's input type to be
// expanded in the docs, but not the resulting type; we do one bit of
// indirection, by creating a {Type}Config type, and setting it with the
// `@inline` and `@expandType {Type}` tags. This makes sure that the config
// options appear right in the documentation so users don't have to move to
// another page to see the fields.
/**
 * Creates a Wasp {@link App}.
 *
 * Call `app()` exactly once in your `main.wasp.ts` and export the result as
 * the file's default export. The Wasp compiler reads this default export to
 * generate your app.
 *
 * See the [Wasp Spec docs](https://wasp.sh/docs/features/spec) for the full
 * shape of the configuration.
 *
 * @example
 * ```ts
 * import { app, page, route } from '@wasp/spec'
 * import MainPage from './src/MainPage' with { type: 'ref' }
 *
 * export default app({
 *   name: 'todoApp',
 *   wasp: { version: "^0.24.0" },
 *   title: "ToDo App",
 *   head: ["<link rel='icon' href='/favicon.ico' />"],
 *   spec: [
 *     route('MainRoute', '/', page(MainPage)),
 *   ],
 * })
 * ```
 *
 * @param config The app configuration.
 *
 * @category Wasp Spec
 */
export function app(config) {
    return config;
}
/**
 * Creates a {@link Page} definition.
 *
 * A page is a React component rendered by a {@link route}.
 *
 * See [Pages & Routes](https://wasp.sh/docs/features/pages-and-routes) and the
 * [Auth overview](https://wasp.sh/docs/features/auth/overview#protecting-a-page-with-authrequired)
 * for protecting pages with `authRequired`.
 *
 * @example
 * ```ts
 * import { page } from '@wasp/spec'
 * import MainPage from './src/MainPage' with { type: 'ref' }
 *
 * page(MainPage, { authRequired: true })
 * ```
 *
 * @param component The React component to render.
 *
 * {@include ./referenceImports.md}
 * @param config Optional page settings such as `authRequired`.
 *
 * @category Constructors
 */
export function page(component, config) {
    return { kind: "page", component, ...config };
}
/**
 * Creates a {@link Route} definition.
 *
 * Maps a URL path to a {@link Page}. Paths support React Router patterns
 * such as dynamic segments (`/tasks/:id`), optional segments
 * (`/photo/:id/edit?`), and splats (`/files/*`).
 *
 * Use `config.prerender` to render the route to static HTML at build time:
 * `true` prerenders the route's own static path, or pass an array of concrete
 * paths to prerender specific instances of a dynamic route (see
 * [Prerendering](https://wasp.sh/docs/advanced/prerendering)). Use
 * `config.lazy` to opt out of lazy-loading the page's bundle.
 *
 * @example
 * ```ts
 * import { page, route } from '@wasp/spec'
 * import MainPage from './src/MainPage' with { type: 'ref' }
 *
 * route('MainRoute', '/', page(MainPage))
 * ```
 *
 * @param name Unique name for the route.
 * @param path URL path the route matches.
 * @param page The result of `page()`.
 * @param config Optional route settings (`lazy`, `prerender`).
 *
 * @category Constructors
 */
export function route(name, path, 
/**
 * @remarks
 * This should be the result of the `page()` function.
 */
page, config) {
    return { kind: "route", name, path, page, ...config };
}
/**
 * Creates a {@link Query} definition.
 *
 * Queries are server-side read-only operations. They can be invoked from
 * the client (with caching via `useQuery`) and the server. List the
 * entities the query reads from so Wasp can inject the matching Prisma
 * delegates into `context.entities` and invalidate the client cache when
 * related actions modify them.
 *
 * See [Queries](https://wasp.sh/docs/features/data/operations/queries).
 *
 * @example
 * ```ts
 * import { app, query } from "@wasp/spec"
 * import { getTasks } from './src/queries' with { type: 'ref' }
 *
 * export default app({
 *   // ...
 *   spec: [
 *     query(getTasks, { entities: ["Foo"] }),
 *   ],
 * })
 * ```
 *
 * @param fn
 *
 * Reference to the Query's NodeJS implementation.
 *
 * See [the
 * docs](https://wasp.sh/docs/features/data/operations/queries#implementing-queries)
 * for details on the implementation and its context.
 *
 * {@include ./referenceImports.md}
 *
 * @param config
 *
 * @category Constructors
 */
export function query(fn, config) {
    return { kind: "query", fn, ...config };
}
/**
 * Creates an {@link Action} definition.
 *
 * Actions are server-side write operations. Like queries, they can be
 * called from the client and the server. Listing entities in `config.entities`
 * lets Wasp invalidate related query caches when this action runs.
 *
 * See [Actions](https://wasp.sh/docs/features/data/operations/actions).
 *
 * @example
 * ```ts
 * import { app, action } from "@wasp/spec"
 * import { createTask } from "./src/actions" with { type: "ref" }
 * export default app({
 *   // ...
 *   spec: [
 *     action(createTask, { entities: ["Task"] }),
 *   ],
 * })
 * ```
 *
 * @param fn
 * Reference to the Action's NodeJS implementation.
 *
 * See [the docs](https://wasp.sh/docs/features/data/operations/actions#implementing-actions) for details on the implementation and its context.
 *
 * {@include ./referenceImports.md}
 * @param config
 *
 * @category Constructors
 */
export function action(fn, config) {
    return { kind: "action", fn, ...config };
}
/**
 * Creates an {@link Api} endpoint definition.
 *
 * APIs are custom HTTP endpoints handled by a plain Express function. Use
 * them for webhooks, file uploads, or any HTTP interaction that doesn't fit
 * the {@link query}/{@link action} model.
 *
 * See [Custom HTTP API Endpoints](https://wasp.sh/docs/features/apis).
 *
 * @example
 * ```ts
 * import { api } from '@wasp/spec'
 * import { barBaz } from './src/apis' with { type: 'ref' }
 *
 * api('GET', '/bar/baz', barBaz, { entities: ['Task'], auth: false })
 * ```
 *
 * @param method HTTP method to listen on (or `"ALL"` for any).
 * @param path Express path the endpoint is mounted at.
 * @param fn The API's NodeJS implementation.
 *
 * {@include ./referenceImports.md}
 * @param config Optional settings: `middlewareConfigFn`, `entities`, `auth`.
 *
 * @category Constructors
 */
export function api(method, path, fn, config) {
    return { kind: "api", method, path, fn, ...config };
}
/**
 * Creates an {@link ApiNamespace} definition.
 *
 * Applies a shared middleware function to every {@link api} mounted under a
 * given path prefix. Useful for tweaking middleware (e.g. raw body parsing,
 * CORS) for a group of related endpoints.
 *
 * See the
 * [per-path middleware section](https://wasp.sh/docs/advanced/server-customization/middleware#3-customize-per-path-middleware).
 *
 * @example
 * ```ts
 * import { apiNamespace } from '@wasp/spec'
 * import { barMiddleware } from './src/apis' with { type: 'ref' }
 *
 * apiNamespace('/bar', { middlewareConfigFn: barMiddleware })
 * ```
 *
 * @param path Path prefix the namespace applies to.
 * @param config Required `middlewareConfigFn`.
 *
 * {@include ./referenceImports.md}
 *
 * @category Constructors
 */
export function apiNamespace(path, config) {
    return { kind: "apiNamespace", path, ...config };
}
/**
 * Creates a {@link Job} definition.
 *
 * Jobs are background tasks that persist across server restarts, can be
 * retried on failure, delayed, and scheduled with cron. Pass the worker
 * function as the first argument and configure the executor and schedule
 * in `config`.
 *
 * See [Recurring Jobs](https://wasp.sh/docs/features/jobs).
 *
 * @example
 * ```ts
 * import { job } from '@wasp/spec'
 * import { foo } from './src/jobs/bar' with { type: 'ref' }
 *
 * job(foo, {
 *   executor: 'PgBoss',
 *   entities: ['Task'],
 *   schedule: { cron: '0 * * * *' },
 * })
 * ```
 *
 * @param fn The async function that performs the job's work. It receives the
 *   submitted args and a context containing the declared entities.
 *
 * {@include ./referenceImports.md}
 * @param config Required `executor` and optional `schedule`, `entities`,
 *   and `performExecutorOptions`.
 *
 * @category Constructors
 */
export function job(fn, config) {
    return { kind: "job", fn, ...config };
}
/**
 * Creates a {@link Crud} definition.
 *
 * Auto-generates queries and actions for a Prisma entity. Each operation in
 * `operations` can be enabled with defaults (an empty object), made public
 * via `isPublic`, or replaced by a custom implementation via `overrideFn`.
 *
 * See [Automatic CRUD](https://wasp.sh/docs/features/data/crud).
 *
 * @example
 * ```ts
 * import { crud } from '@wasp/spec'
 * import { createTaskOverride } from './src/actions' with { type: 'ref' }
 *
 * crud('tasks', 'Task', {
 *   getAll: { isPublic: true },
 *   get: {},
 *   create: { overrideFn: createTaskOverride },
 *   update: {},
 * })
 * ```
 *
 * @param name Unique name for the generated CRUD.
 * @param entity Name of the Prisma entity to generate operations for.
 * @param operations Which operations to generate and how to configure each.
 *
 * {@include ./referenceImports.md}
 *
 * @category Constructors
 */
export function crud(name, entity, operations) {
    return { kind: "crud", name, entity, operations };
}
