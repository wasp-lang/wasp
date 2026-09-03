import type { AnyFunction, AnyObject } from "../../typeUtils.js";
import {
  reservedClientEnvVarNames,
  reservedServerEnvVarNames,
} from "../authReservedEnvVarNames.js";
import { WaspSpecUserError } from "../waspSpecUserError.js";
import type {
  Action,
  Api,
  ApiNamespace,
  App,
  AuthProviderManifest,
  AuthRuntimeGrantName,
  Crud,
  EnvVarRequirement,
  Job,
  Page,
  Query,
  Reference,
  Route,
} from "./waspSpec.js";

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
export function app(config: AppConfig): App {
  return config;
}

/**
 * The configuration object accepted by the {@link app} constructor.
 *
 * @category Wasp Spec
 *
 * @inline
 * @expandType App
 */
export type AppConfig = Omit<App, "kind">;

/**
 * Creates a {@link Page} definition.
 *
 * A page is a React component rendered by a {@link route}.
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
 *
 * {@include ./referenceImports.md}
 * @param config Optional page settings such as `authRequired`.
 *
 * @category Constructors
 */
export function page(component: Page["component"], config?: PageConfig): Page {
  return { kind: "page", component, ...config };
}

/**
 * The optional configuration object accepted as the last argument of the
 * {@link page} constructor.
 *
 * @category Constructors
 *
 * @inline
 * @expandType Page
 */
export type PageConfig = Omit<Page, "kind" | "component">;

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
 * The optional configuration object accepted as the last argument of the
 * {@link route} constructor.
 *
 * @category Constructors
 *
 * @inline
 * @expandType Route
 */
export type RouteConfig = Omit<Route, "kind" | "name" | "path" | "page">;

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
 * import { app, query } from "@wasp.sh/spec"
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
 * docs](https://wasp.sh/docs/data-model/operations/queries#implementing-queries)
 * for details on the implementation and its context.
 *
 * {@include ./referenceImports.md}
 *
 * @param config
 *
 * @category Constructors
 */
export function query(fn: Query["fn"], config?: QueryConfig): Query {
  return { kind: "query", fn, ...config };
}

/**
 * The optional configuration object accepted as the last argument of the
 * {@link query} constructor.
 *
 * @category Constructors
 *
 * @inline
 * @expandType Query
 */
export type QueryConfig = Omit<Query, "kind" | "fn">;

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
 * import { app, action } from "@wasp.sh/spec"
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
 * See [the docs](https://wasp.sh/docs/data-model/operations/actions#implementing-actions) for details on the implementation and its context.
 *
 * {@include ./referenceImports.md}
 * @param config
 *
 * @category Constructors
 */
export function action(fn: Action["fn"], config?: ActionConfig): Action {
  return { kind: "action", fn, ...config };
}

/**
 * The optional configuration object accepted as the last argument of the
 * {@link action} constructor.
 *
 * @category Constructors
 *
 * @inline
 * @expandType Action
 */
export type ActionConfig = Omit<Action, "kind" | "fn">;

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
 *
 * {@include ./referenceImports.md}
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
 * The optional configuration object accepted as the last argument of the
 * {@link api} constructor.
 *
 * @category Constructors
 *
 * @inline
 * @expandType Api
 */
export type ApiConfig = Omit<Api, "kind" | "method" | "path" | "fn">;

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
 * {@include ./referenceImports.md}
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
 * The configuration object accepted as the last argument of the
 * {@link apiNamespace} constructor.
 *
 * @category Constructors
 *
 * @inline
 * @expandType ApiNamespace
 */
export type ApiNamespaceConfig = Omit<ApiNamespace, "kind" | "path">;

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
 *
 * {@include ./referenceImports.md}
 * @param config Required `executor` and optional `schedule`, `entities`,
 *   and `performExecutorOptions`.
 *
 * @category Constructors
 */
export function job(fn: Job["fn"], config: JobConfig): Job {
  return { kind: "job", fn, ...config };
}

/**
 * The configuration object accepted as the last argument of the
 * {@link job} constructor.
 *
 * @category Constructors
 *
 * @inline
 * @expandType Job
 */
export type JobConfig = Omit<Job, "kind" | "fn">;

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
export function crud(
  name: Crud["name"],
  entity: Crud["entity"],
  operations: Crud["operations"],
): Crud {
  return { kind: "crud", name, entity, operations };
}

/**
 * The input accepted by {@link defineAuthProviderManifest}: everything in the
 * manifest that carries information, without the fields the definition step
 * fills in itself (`kind`, `contractVersion`, the authenticity marker).
 *
 * @category Experimental
 */
export type AuthProviderManifestInput = Omit<
  AuthProviderManifest,
  | "kind"
  | "contractVersion"
  | "__waspAuthProviderManifest"
  | "capabilities"
  | "env"
  | "uses"
  | "identityNamespaces"
  | "extensions"
> & {
  capabilities?: string[];
  env?: { server?: EnvVarRequirement[]; client?: EnvVarRequirement[] };
  uses?: AuthRuntimeGrantName[];
  identityNamespaces?: string[];
  extensions?: Record<string, Reference<AnyFunction | AnyObject>>;
};

/**
 * EXPERIMENTAL. Defines an external auth provider manifest.
 *
 * This is the function auth adapter packages call from their spec helpers
 * (`clerk()`, `betterAuth()`, ...). It normalizes the manifest, validates it,
 * and stamps it as authentic -- the compiler rejects hand-crafted manifest
 * object literals so that every manifest in circulation went through these
 * checks.
 *
 * App developers normally never call this directly: use an adapter package's
 * spec helper, or {@link customAuthProvider} for a hand-written adapter.
 *
 * @category Experimental
 */
export function defineAuthProviderManifest(
  manifest: AuthProviderManifestInput,
): AuthProviderManifest {
  if (!isValidProviderId(manifest.id)) {
    throw new WaspSpecUserError(
      `Auth provider id '${String(manifest.id)}' must be non-empty and contain no ':' -- the ':' separates a provider id from its identity namespaces ('wasp:email').`,
    );
  }
  if (
    manifest.routes !== undefined &&
    !manifest.routes.basePath.startsWith("/")
  ) {
    throw new WaspSpecUserError(
      `Auth provider '${manifest.id}' declares routes with a basePath that does not start with '/': '${manifest.routes.basePath}'.`,
    );
  }

  const capabilities = manifest.capabilities ?? [];
  // The plan doc's must-enforce caveat: a cookie-borne session Wasp cannot
  // revoke server-side would make `logout()` a lie, so the combination is
  // rejected at definition time rather than documented.
  if (
    capabilities.includes("cookie-transport") &&
    !capabilities.includes("session-revocation")
  ) {
    throw new WaspSpecUserError(
      `Auth provider '${manifest.id}' declares the 'cookie-transport' capability without 'session-revocation'. A provider whose credential lives in a cookie must be able to revoke sessions server-side, or logout would only appear to work.`,
    );
  }

  // Adapters receive exactly the env vars they declared, so declaring a
  // framework-owned name would hand the adapter framework secrets (JWT_SECRET)
  // through the sanctioned channel.
  for (const [side, envVars] of [
    ["server", manifest.env?.server ?? []],
    ["client", manifest.env?.client ?? []],
  ] as const) {
    const reservedNames =
      side === "server" ? reservedServerEnvVarNames : reservedClientEnvVarNames;
    for (const envVar of envVars) {
      if (reservedNames.includes(envVar.name)) {
        throw new WaspSpecUserError(
          `Auth provider '${manifest.id}' declares the ${side} env var '${envVar.name}', which Wasp owns. Framework env var names cannot be declared by providers; pick a provider-specific name.`,
        );
      }
    }
  }

  const uses = manifest.uses ?? [];
  for (const grant of uses) {
    if (!knownRuntimeGrantNames.includes(grant)) {
      throw new WaspSpecUserError(
        `Auth provider '${manifest.id}' requests the unknown runtime grant '${String(
          grant,
        )}'. Known grants: ${knownRuntimeGrantNames.join(", ")}.`,
      );
    }
  }

  const identityNamespaces = manifest.identityNamespaces ?? [manifest.id];
  validateIdentityNamespaces(manifest.id, identityNamespaces, uses);

  return {
    ...manifest,
    kind: "external",
    contractVersion: 1,
    capabilities,
    env: {
      server: manifest.env?.server ?? [],
      client: manifest.env?.client ?? [],
    },
    uses,
    identityNamespaces,
    extensions: manifest.extensions ?? {},
    __waspAuthProviderManifest: true,
  } as AuthProviderManifest;
}

/**
 * A provider id names an identity namespace, and ':' is the separator between
 * a provider id and its sub-namespaces ('wasp:email'), so an id cannot carry
 * one.
 */
export function isValidProviderId(id: unknown): id is string {
  return typeof id === "string" && id.length > 0 && !id.includes(":");
}

const knownRuntimeGrantNames: readonly AuthRuntimeGrantName[] = [
  "wasp-sessions",
  "email-send",
  "identity-namespaces",
];

// Shared by defineAuthProviderManifest and the mapper (which re-validates,
// because the authenticity marker is forgeable as a plain property).
export function validateIdentityNamespaces(
  providerId: string,
  identityNamespaces: readonly string[],
  uses: readonly string[],
): void {
  for (const namespace of identityNamespaces) {
    const isOwnNamespace =
      namespace === providerId ||
      (namespace.startsWith(`${providerId}:`) &&
        namespace.length > providerId.length + 1);
    if (!isOwnNamespace) {
      throw new WaspSpecUserError(
        `Auth provider '${providerId}' declares the identity namespace '${namespace}', which it does not own. A namespace must be the provider id or '${providerId}:<suffix>' -- that rule is what makes cross-provider identity collisions impossible.`,
      );
    }
  }
  if (new Set(identityNamespaces).size !== identityNamespaces.length) {
    throw new WaspSpecUserError(
      `Auth provider '${providerId}' declares a duplicate identity namespace.`,
    );
  }
  const usesBeyondDefault =
    identityNamespaces.length > 1 ||
    (identityNamespaces.length === 1 && identityNamespaces[0] !== providerId);
  if (usesBeyondDefault && !uses.includes("identity-namespaces")) {
    throw new WaspSpecUserError(
      `Auth provider '${providerId}' declares identity namespaces beyond its default one, which requires the 'identity-namespaces' grant in \`uses\`.`,
    );
  }
}

/**
 * The configuration accepted by {@link customAuthProvider}.
 *
 * @category Experimental
 *
 * @inline
 */
export type CustomAuthProviderConfig = {
  /**
   * Stable identifier of the provider. Identities Wasp provisions for this
   * provider's subjects are recorded under this name, so it must stay stable
   * across deploys, and it must match the `id` of the `AuthProvider` object
   * the `server` module exports.
   */
  id: string;
  /** Reference to a user-code module exporting an `AuthProvider` object. */
  server: Reference<AnyObject>;
  /** See {@link AuthProviderManifest.capabilities}. */
  capabilities?: string[];
  /** See {@link AuthProviderManifest.env}. */
  env?: { server?: EnvVarRequirement[]; client?: EnvVarRequirement[] };
  /** See {@link AuthProviderManifest.uses}. */
  uses?: AuthRuntimeGrantName[];
  /** See {@link AuthProviderManifest.identityNamespaces}. */
  identityNamespaces?: string[];
  /** See {@link AuthProviderManifest.userSignupFields}. */
  userSignupFields?: Reference<AnyObject>;
  /** See {@link AuthProviderManifest.options}. */
  options?: unknown;
  /** See {@link AuthProviderManifest.extensions}. */
  extensions?: Record<string, Reference<AnyFunction | AnyObject>>;
};

/**
 * EXPERIMENTAL. Declares a hand-written external auth provider: an
 * `AuthProvider` implementation living in the app's own `src/`, referenced the
 * same way as any other user code.
 *
 * This is the escape hatch under every adapter package -- anything a package
 * can do, an app can do locally. Prefer a published `@wasp.sh/auth-*` (or
 * community) adapter package when one exists for your provider.
 *
 * @example
 * ```ts
 * import { customAuthProvider } from '@wasp.sh/spec'
 * import { myAuthProvider } from './src/auth/provider' with { type: 'ref' }
 *
 * auth: {
 *   userEntity: "User",
 *   onAuthFailedRedirectTo: "/login",
 *   providers: [
 *     customAuthProvider({ id: "my-provider", server: myAuthProvider }),
 *   ],
 * }
 * ```
 *
 * @category Experimental
 */
export function customAuthProvider(
  config: CustomAuthProviderConfig,
): AuthProviderManifest {
  return defineAuthProviderManifest(config);
}
