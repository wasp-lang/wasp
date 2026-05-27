import type {
  Action,
  Api,
  ApiNamespace,
  App,
  Crud,
  Job,
  Page,
  Query,
  Route,
} from "./tsAppSpec.js";

// Throughout this file, in order for the constructor's input type to be
// expanded in the docs, but not the resulting type; we do one bit of
// indirection, by creating a {Type}Config file, and setting it with the
// `@inline` and `@expandType {Type}` tags. This makes sure that the config
// option appear right in the documentation of the so users don't have to move
// to another page to see the fields.

/**
 * Creates a Wasp {@link App}.
 *
 * Call `app()` exactly once in your `main.wasp.ts` and export the result as
 * the file's default export. The Wasp compiler reads this default export to
 * generate your app.
 *
 * See the [Wasp Spec docs](https://wasp.sh/docs/general/spec) for the full
 * shape of the configuration.
 *
 * @example
 * ```ts
 * import { app, page, route } from '@wasp.sh/spec'
 * import MainPage from './src/MainPage' with { type: 'ref' }
 *
 * export default app({
 *   name: 'todoApp',
 *   title: 'ToDo App',
 *   wasp: { version: '^0.24.0' },
 *   parts: [
 *     route('MainRoute', '/', page(MainPage)),
 *   ],
 * })
 * ```
 *
 * @param config The app configuration.
 *
 * @category Spec
 */
export function app(config: AppConfig): App {
  return config;
}

/**
 * @inline
 * @expandType App
 */
type AppConfig = Omit<App, "kind">;

/**
 * Creates a {@link Page} definition.
 *
 * A page is a React component rendered by a {@link route}. Pass the
 * component as the first argument, either via a reference import
 * (recommended) or an {@link ExtImport} object.
 *
 * See [Routing](https://wasp.sh/docs/advanced/routing) and the
 * [Auth overview](https://wasp.sh/docs/auth/overview#protecting-a-page-with-authrequired)
 * for protecting pages with `authRequired`.
 *
 * @example
 * ```ts
 * import { page } from '@wasp.sh/spec'
 * import MainPage from './src/MainPage' with { type: 'ref' }
 *
 * page(MainPage, { authRequired: true })
 * ```
 *
 * @param component The React component to render.
 * @param config Optional page settings such as `authRequired`.
 *
 * @category Constructors
 */
export function page(component: Page["component"], config?: PageConfig): Page {
  return { kind: "page", component, ...config };
}

/**
 * @inline
 * @expandType Page
 */
type PageConfig = Omit<Page, "kind" | "component">;

/**
 * Creates a {@link Route} definition.
 *
 * Maps a URL path to a {@link Page}. Paths support React Router patterns
 * such as dynamic segments (`/tasks/:id`), optional segments
 * (`/photo/:id/edit?`), and splats (`/files/*`).
 *
 * Use `config.prerender` to render the route to static HTML at build time
 * (see [Prerendering](https://wasp.sh/docs/advanced/prerendering)) and
 * `config.lazy` to opt out of lazy-loading the page's bundle.
 *
 * @example
 * ```ts
 * import { page, route } from '@wasp.sh/spec'
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
export function route(
  name: Route["name"],
  path: Route["path"],
  /**
   * @remarks
   * This should be the result of the `page()` function.
   */
  page: Route["page"],
  config?: RouteConfig,
): Route {
  return { kind: "route", name, path, page, ...config };
}

/**
 * @inline
 * @expandType Route
 */
type RouteConfig = Omit<Route, "kind" | "name" | "path" | "page">;

/**
 * Creates a {@link Query} definition.
 *
 * Queries are server-side read-only operations. They can be invoked from
 * the client (with caching via `useQuery`) and the server. List the
 * entities the query reads from so Wasp can inject the matching Prisma
 * delegates into `context.entities` and invalidate the client cache when
 * related actions modify them.
 *
 * See [Queries](https://wasp.sh/docs/data-model/operations/queries).
 *
 * @example
 * ```ts
 * import { query } from '@wasp.sh/spec'
 * import { getTasks } from './src/queries' with { type: 'ref' }
 *
 * query(getTasks, { entities: ['Task'] })
 * ```
 *
 * @param fn The Query's NodeJS implementation.
 * @param config Optional settings: `entities` and `auth`.
 *
 * @category Constructors
 */
export function query(fn: Query["fn"], config?: QueryConfig): Query {
  return { kind: "query", fn, ...config };
}

/**
 * @inline
 * @expandType Query
 */
type QueryConfig = Omit<Query, "kind" | "fn">;

/**
 * Creates an {@link Action} definition.
 *
 * Actions are server-side write operations. Like queries, they can be
 * called from the client and the server. Listing entities in `config.entities`
 * lets Wasp invalidate related query caches when this action runs.
 *
 * See [Actions](https://wasp.sh/docs/data-model/operations/actions).
 *
 * @example
 * ```ts
 * import { action } from '@wasp.sh/spec'
 * import { createTask } from './src/actions' with { type: 'ref' }
 *
 * action(createTask, { entities: ['Task'] })
 * ```
 *
 * @param fn The Action's NodeJS implementation.
 * @param config Optional settings: `entities` and `auth`.
 *
 * @category Constructors
 */
export function action(fn: Action["fn"], config?: ActionConfig): Action {
  return { kind: "action", fn, ...config };
}

/**
 * @inline
 * @expandType Action
 */
type ActionConfig = Omit<Action, "kind" | "fn">;

/**
 * Creates an {@link Api} endpoint definition.
 *
 * APIs are custom HTTP endpoints handled by a plain Express function. Use
 * them for webhooks, file uploads, or any HTTP interaction that doesn't fit
 * the {@link query}/{@link action} model.
 *
 * See [Custom HTTP API Endpoints](https://wasp.sh/docs/advanced/apis).
 *
 * @example
 * ```ts
 * import { api } from '@wasp.sh/spec'
 * import { barBaz } from './src/apis' with { type: 'ref' }
 *
 * api('GET', '/bar/baz', barBaz, { entities: ['Task'], auth: false })
 * ```
 *
 * @param method HTTP method to listen on (or `"ALL"` for any).
 * @param path Express path the endpoint is mounted at.
 * @param fn The API's NodeJS implementation.
 * @param config Optional settings: `middlewareConfigFn`, `entities`, `auth`.
 *
 * @category Constructors
 */
export function api(
  method: Api["method"],
  path: Api["path"],
  fn: Api["fn"],
  config?: ApiConfig,
): Api {
  return { kind: "api", method, path, fn, ...config };
}

/**
 * @inline
 * @expandType Api
 */
type ApiConfig = Omit<Api, "kind" | "method" | "path" | "fn">;

/**
 * Creates an {@link ApiNamespace} definition.
 *
 * Applies a shared middleware function to every {@link api} mounted under a
 * given path prefix. Useful for tweaking middleware (e.g. raw body parsing,
 * CORS) for a group of related endpoints.
 *
 * See the
 * [per-path middleware section](https://wasp.sh/docs/advanced/middleware-config#3-customize-per-path-middleware).
 *
 * @example
 * ```ts
 * import { apiNamespace } from '@wasp.sh/spec'
 * import { barMiddleware } from './src/apis' with { type: 'ref' }
 *
 * apiNamespace('/bar', { middlewareConfigFn: barMiddleware })
 * ```
 *
 * @param path Path prefix the namespace applies to.
 * @param config Required `middlewareConfigFn`.
 *
 * @category Constructors
 */
export function apiNamespace(
  path: ApiNamespace["path"],
  config: ApiNamespaceConfig,
): ApiNamespace {
  return { kind: "apiNamespace", path, ...config };
}

/**
 * @inline
 * @expandType ApiNamespace
 */
type ApiNamespaceConfig = Omit<ApiNamespace, "kind" | "path">;

/**
 * Creates a {@link Job} definition.
 *
 * Jobs are background tasks that persist across server restarts, can be
 * retried on failure, delayed, and scheduled with cron. Pass the worker
 * function as the first argument and configure the executor and schedule
 * in `config`.
 *
 * See [Recurring Jobs](https://wasp.sh/docs/advanced/jobs).
 *
 * @example
 * ```ts
 * import { job } from '@wasp.sh/spec'
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
 * @param config Required `executor` and optional `schedule`, `entities`,
 *   and `performExecutorOptions`.
 *
 * @category Constructors
 */
export function job(fn: Job["fn"], config: JobConfig): Job {
  return { kind: "job", fn, ...config };
}

/**
 * @inline
 * @expandType Job
 */
type JobConfig = Omit<Job, "kind" | "fn">;

/**
 * Creates a {@link Crud} definition.
 *
 * Auto-generates queries and actions for a Prisma entity. Each operation in
 * `operations` can be enabled with defaults (an empty object), made public
 * via `isPublic`, or replaced by a custom implementation via `overrideFn`.
 *
 * See [Automatic CRUD](https://wasp.sh/docs/data-model/crud).
 *
 * @example
 * ```ts
 * import { crud } from '@wasp.sh/spec'
 * import { createTaskOverride } from './src/actions' with { type: 'ref' }
 *
 * crud('tasks', 'Task', {
 *   getAll: {},
 *   create: { overrideFn: createTaskOverride },
 * })
 * ```
 *
 * @param name Unique name for the generated CRUD.
 * @param entity Name of the Prisma entity to generate operations for.
 * @param operations Which operations to generate and how to configure each.
 *
 * @category Constructors
 */
export function crud(
  name: Crud["name"],
  entity: Crud["entity"],
  operations: Crud["operations"],
): Crud {
  return { kind: "crud", name, entity, operations };
}
