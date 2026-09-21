import type { AnyObject } from "../../typeUtils.js";
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
  AuthRuntimeGrantName,
  AuthSchemeClientSide,
  AuthSchemeManifest,
  AuthSchemeServerSide,
  CredentialsConfig,
  CredentialStore,
  CredentialTransport,
  Crud,
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
 * See the [Wasp Spec docs](https://wasp.sh/docs/features/spec) for the full
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
 * See [Routing](https://wasp.sh/docs/features/routes) and the
 * [Auth overview](https://wasp.sh/docs/features/auth/overview#protecting-a-page-with-authrequired)
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
 * See [Queries](https://wasp.sh/docs/features/data/operations/queries).
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
 * docs](https://wasp.sh/docs/features/data/operations/queries#implementing-queries)
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
 * See [Actions](https://wasp.sh/docs/features/data/operations/actions).
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
 * See [the docs](https://wasp.sh/docs/features/data/operations/actions#implementing-actions) for details on the implementation and its context.
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
 * See [Custom HTTP API Endpoints](https://wasp.sh/docs/features/apis).
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
 * [per-path middleware section](https://wasp.sh/docs/advanced/server-customization/middleware#3-customize-per-path-middleware).
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
 * See [Recurring Jobs](https://wasp.sh/docs/features/jobs).
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
 * See [Automatic CRUD](https://wasp.sh/docs/features/data/crud).
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
 * The input accepted by {@link defineAuthSchemeManifest}: everything in the
 * manifest that carries information, without the fields the definition step
 * fills in itself (`kind`, `contractVersion`, the authenticity marker).
 *
 * @category Experimental
 */
export type AuthSchemeManifestInput = Omit<
  AuthSchemeManifest,
  "kind" | "contractVersion" | "__waspAuthSchemeManifest" | "capabilities"
> & {
  capabilities?: string[];
};

/**
 * EXPERIMENTAL. Defines an auth scheme manifest.
 *
 * This is the function auth handler packages call from their spec helpers
 * (`waspAuth()`, `clerk()`, `betterAuth()`, ...). It normalizes the manifest,
 * validates it, and stamps it as authentic -- the compiler rejects
 * hand-crafted manifest object literals so that every manifest in circulation
 * went through these checks.
 *
 * App developers normally never call this directly: use a handler package's
 * spec helper, or {@link customAuthHandler} for a hand-written handler.
 *
 * @category Experimental
 */
export function defineAuthSchemeManifest(
  manifest: AuthSchemeManifestInput,
): AuthSchemeManifest {
  if (typeof manifest.server !== "object" || manifest.server === null) {
    throw new WaspSpecUserError(
      "An auth scheme manifest must describe its server half (`server`).",
    );
  }
  const handler = describeAuthHandler(manifest);
  validateAuthAdapterEntry(handler, "server", manifest.server);
  if (manifest.client !== undefined) {
    validateAuthAdapterEntry(handler, "client", manifest.client);
  }

  // Handlers receive exactly the env vars they declared, so declaring a
  // framework-owned name would hand the handler framework secrets (DATABASE_URL)
  // through the sanctioned channel.
  validateSideEnvVars(handler, manifest);

  for (const grant of manifest.uses ?? []) {
    if (!knownRuntimeGrantNames.includes(grant)) {
      throw new WaspSpecUserError(
        `Auth handler '${handler}' requests the unknown runtime grant '${String(
          grant,
        )}'. Known grants: ${knownRuntimeGrantNames.join(", ")}.`,
      );
    }
  }

  validateIdentityNamespaces(handler, manifest.identityNamespaces ?? []);

  if (manifest.credentials !== undefined) {
    validateCredentialsConfig(handler, manifest.credentials);
  }

  return {
    ...manifest,
    kind: "scheme",
    contractVersion: supportedAuthContractVersion,
    capabilities: manifest.capabilities ?? [],
    __waspAuthSchemeManifest: true,
  };
}

/**
 * The one auth contract version this compiler understands. A manifest carries
 * the version its handler was built against; the mapper rejects any other, so
 * handler/compiler skew is a clear error instead of a silently ignored field.
 * Used for the stamped value, the check and its message, so they cannot drift.
 */
export const supportedAuthContractVersion = 7 as const;

/**
 * A label for error messages: where the server half's code lives. The package
 * specifier, or the import path of a hand-written adapter -- more useful than
 * a made-up name, and it cannot go stale.
 */
export function describeAuthHandler(
  manifest: Pick<AuthSchemeManifestInput, "server">,
): string {
  const entry = manifest.server?.authAdapter as
    | { package?: unknown; from?: unknown }
    | undefined;
  if (typeof entry?.package === "string") return entry.package;
  if (typeof entry?.from === "string") return entry.from;
  return "unknown";
}

function validateAuthAdapterEntry(
  handler: string,
  side: "server" | "client",
  sideManifest: { authAdapter?: unknown },
): void {
  const entry = sideManifest.authAdapter;
  if (typeof entry !== "object" || entry === null) {
    throw new WaspSpecUserError(
      `Auth handler '${handler}' must say where its ${side} half lives (\`${side}.authAdapter\`): { package, export? } or a reference to an adapter in your code.`,
    );
  }
  if ("package" in entry) {
    const { package: packageSpecifier, export: exportName } = entry as {
      package: unknown;
      export?: unknown;
    };
    if (typeof packageSpecifier !== "string" || packageSpecifier.length === 0) {
      throw new WaspSpecUserError(
        `Auth handler '${handler}' has an empty ${side}.authAdapter.package.`,
      );
    }
    // It is interpolated into generated `import { <export> } from` code.
    if (
      exportName !== undefined &&
      (typeof exportName !== "string" ||
        !/^[A-Za-z_$][A-Za-z0-9_$]*$/.test(exportName))
    ) {
      throw new WaspSpecUserError(
        `Auth handler '${handler}' has an invalid ${side}.authAdapter.export '${String(exportName)}': it must be a JavaScript identifier.`,
      );
    }
  }
}

// Shared by defineAuthSchemeManifest and the mapper (which re-validates,
// because the authenticity marker is forgeable as a plain property).
export function validateSideEnvVars(
  handler: string,
  manifest: Pick<AuthSchemeManifestInput, "server" | "client">,
): void {
  for (const [side, envVars, reservedNames] of [
    ["server", manifest.server.env ?? [], reservedServerEnvVarNames],
    ["client", manifest.client?.env ?? [], reservedClientEnvVarNames],
  ] as const) {
    for (const envVar of envVars) {
      if (reservedNames.includes(envVar.name)) {
        throw new WaspSpecUserError(
          `Auth handler '${handler}' declares the ${side} env var '${envVar.name}', which Wasp owns. Framework env var names cannot be declared by handlers; pick a handler-specific name.`,
        );
      }
    }
  }
}

/**
 * A scheme name is an identity namespace and a route segment, so it cannot
 * carry the namespace separator ':' or a '/'.
 */
export function isValidSchemeName(name: unknown): name is string {
  return (
    typeof name === "string" &&
    name.length > 0 &&
    !name.includes(":") &&
    !name.includes("/")
  );
}

const knownRuntimeGrantNames: readonly AuthRuntimeGrantName[] = ["email-send"];

// Shared by defineAuthSchemeManifest and the mapper (which re-validates,
// because the authenticity marker is forgeable as a plain property).
export function validateIdentityNamespaces(
  handler: string,
  identityNamespaces: readonly string[],
): void {
  for (const suffix of identityNamespaces) {
    if (suffix.length === 0 || suffix.includes(":")) {
      throw new WaspSpecUserError(
        `Auth handler '${handler}' declares the identity namespace suffix '${suffix}', which must be non-empty and contain no ':' -- Wasp prefixes it with the scheme name ('<scheme>:${suffix}').`,
      );
    }
  }
  if (new Set(identityNamespaces).size !== identityNamespaces.length) {
    throw new WaspSpecUserError(
      `Auth handler '${handler}' declares a duplicate identity namespace.`,
    );
  }
}

const knownCredentialTransports = ["bearer", "cookie"] as const;
const knownCredentialStores = ["prisma", "signed-token"] as const;

// Shared by defineAuthSchemeManifest and the mapper.
export function validateCredentialsConfig(
  handler: string,
  credentials: CredentialsConfig,
): void {
  if (typeof credentials !== "object" || credentials === null) {
    throw new WaspSpecUserError(
      `Auth handler '${handler}' declares invalid credentials: expected { scheme } or { transport, store }.`,
    );
  }
  if ("scheme" in credentials) {
    if (!isValidSchemeName(credentials.scheme)) {
      throw new WaspSpecUserError(
        `Auth handler '${handler}' declares credentials.scheme '${String(credentials.scheme)}', which is not a valid scheme name.`,
      );
    }
    return;
  }
  if (
    credentials.transport !== undefined &&
    !knownCredentialTransports.includes(credentials.transport)
  ) {
    throw new WaspSpecUserError(
      `Auth handler '${handler}' declares the unknown credential transport '${String(credentials.transport)}'. Known: ${knownCredentialTransports.join(", ")}.`,
    );
  }
  if (
    typeof credentials.store === "string" &&
    !(knownCredentialStores as readonly string[]).includes(credentials.store)
  ) {
    throw new WaspSpecUserError(
      `Auth handler '${handler}' declares the unknown credential store '${credentials.store}'. Known: ${knownCredentialStores.join(", ")}, or a reference to your own store.`,
    );
  }
}

/**
 * The configuration accepted by {@link customAuthHandler}.
 *
 * @category Experimental
 *
 * @inline
 */
export type CustomAuthHandlerConfig = {
  /**
   * The server half. `authAdapter` is a reference to a
   * `ServerAuthAdapter` in the app's own code: a function that
   * receives the scheme's runtime and returns `{ handler, routeHandler? }`,
   * exactly like a handler package's `createServerAuthHandler`.
   */
  server: AuthSchemeServerSide;
  /**
   * The client half. `authAdapter` is a reference to a
   * `ClientAuthAdapter` in the app's own code, exactly like a
   * handler package's `createClientAuthHandler`.
   */
  client?: AuthSchemeClientSide;
  /** See {@link AuthSchemeManifest.capabilities}. */
  capabilities?: string[];
  /** See {@link AuthSchemeManifest.uses}. */
  uses?: AuthRuntimeGrantName[];
  /** See {@link AuthSchemeManifest.identityNamespaces}. */
  identityNamespaces?: string[];
  /** See {@link AuthSchemeManifest.credentials}. */
  credentials?: CredentialsConfig;
  /** See {@link AuthSchemeManifest.userFieldsFromClaims}. */
  userFieldsFromClaims?: Reference<AnyObject>;
};

/**
 * EXPERIMENTAL. Declares a hand-written auth handler: an `AuthHandler`
 * implementation living in the app's own `src/`, referenced the same way as
 * any other user code.
 *
 * This is the escape hatch under every handler package -- anything a package
 * can do, an app can do locally. Prefer a published `@wasp.sh/auth-*` (or
 * community) package when one exists for your provider.
 *
 * @example
 * ```ts
 * import { customAuthHandler } from '@wasp.sh/spec'
 * import { myAuthHandler } from './src/auth/handler' with { type: 'ref' }
 *
 * auth: {
 *   userEntity: "User",
 *   onAuthFailedRedirectTo: "/login",
 *   schemes: {
 *     password: customAuthHandler({
 *       server: { authAdapter: createMyAuthHandler },
 *     }),
 *   },
 * }
 * ```
 *
 * @category Experimental
 */
export function customAuthHandler(
  config: CustomAuthHandlerConfig,
): AuthSchemeManifest {
  return defineAuthSchemeManifest(config);
}

/**
 * The configuration accepted by {@link waspBearer} and {@link waspCookie}.
 *
 * @category Auth
 *
 * @inline
 */
export type WaspCredentialSchemeConfig = {
  /** See {@link CredentialStore}. Default: `"prisma"`. */
  store?: CredentialStore;
  /** Credential lifetime, e.g. `"30d"` or `"15m"`. Default: 30 days. */
  ttl?: string;
};

/**
 * A scheme that issues Wasp bearer credentials: a token the generated client
 * attaches to every request. Other schemes sign into it with
 * `credentials: { scheme: "<its name>" }`; declare it standalone when several
 * schemes should share one credential.
 *
 * @example
 * ```ts
 * schemes: {
 *   session: waspBearer({ store: "prisma" }),
 *   wasp: waspAuth({ methods: { email: {...} }, credentials: { scheme: "session" } }),
 *   clerk: clerk({ credentials: { scheme: "session" } }),
 * },
 * default: "session",
 * ```
 *
 * @category Auth
 */
export function waspBearer(
  config: WaspCredentialSchemeConfig = {},
): AuthSchemeManifest {
  return waspCredentialScheme("bearer", config);
}

/**
 * A scheme that issues Wasp cookie credentials: an `HttpOnly` cookie the
 * browser attaches on its own. Same-site deployments only. See
 * {@link waspBearer} for how other schemes sign into it.
 *
 * @category Auth
 */
export function waspCookie(
  config: WaspCredentialSchemeConfig = {},
): AuthSchemeManifest {
  return waspCredentialScheme("cookie", config);
}

function waspCredentialScheme(
  transport: CredentialTransport,
  config: WaspCredentialSchemeConfig,
): AuthSchemeManifest {
  const store = config.store ?? "prisma";
  // The generated client already stores a bearer credential a sibling scheme
  // adopts, so the issuer has no client entry of its own.
  return defineAuthSchemeManifest({
    server: {
      authAdapter: { package: "wasp/server/auth/issuer" },
      env:
        store === "signed-token"
          ? [
              {
                name: "WASP_CREDENTIAL_SECRET",
                doc: "Signs Wasp credentials. openssl rand -base64 32",
                devDefault: "DEVCREDENTIALSECRET",
              },
            ]
          : [],
    },
    capabilities: [
      "sign-in",
      ...(transport === "cookie" ? ["cookie-transport"] : []),
    ],
    // The scheme IS its issuer: the compiler reads these and builds it
    // without a handler package in between.
    credentials: { transport, store, ttl: config.ttl ?? "30d" },
  });
}
