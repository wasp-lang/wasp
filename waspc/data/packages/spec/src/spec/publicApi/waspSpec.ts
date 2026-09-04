/* eslint-disable @typescript-eslint/no-empty-object-type */
import type { AnyFunction, AnyObject } from "../../typeUtils.js";
import type { RefObject } from "../refObject.js";
import { FromRegister } from "./register.js";

/**
 * Root shape of a Wasp app specification.
 *
 * Pass an `App` to the {@link app} constructor and `export default` the
 * result from `main.wasp.ts`.
 *
 * @category Wasp Spec
 *
 * @example
 * ```ts
 * import { app } from "@wasp.sh/spec"
 *
 * export default app({
 *   name: "todoApp",
 *   wasp: { version: "^0.24.0" },
 *   title: "ToDo App",
 *   head: ["<link rel='icon' href='/favicon.ico' />"],
 *   spec: [],
 * });
 * ```
 */
export interface App {
  /**
   * Internal app name.
   *
   * Must not contain spaces.
   */
  name: string;
  /**
   * Wasp metadata.
   *
   * This will get checked against the Wasp CLI.
   */
  wasp: Wasp;
  /**
   * App title.
   *
   * Used as the browser tab title.
   */
  title: string;
  /**
   * Extra tags injected into the HTML `<head>`.
   *
   * Each entry is rendered inside a React component, so the strings must be
   * valid JSX: self-closing tags must end with `/>` (e.g. `<meta ... />`),
   * and attributes must be camelCased (e.g. `httpEquiv` instead of
   * `http-equiv`).
   *
   * Due to a [React bug](https://github.com/facebook/react/issues/36169),
   * avoid `defer` on `<script>` tags because it can cause hydration
   * warnings. Use `async` instead.
   */
  head?: string[];
  /** Configuration for authentication. Enables auth when set. */
  auth?: Auth;
  /** Configuration for the server part of the resulting Wasp app. */
  server?: Server;
  /** Configuration for the client part of the resulting Wasp app. */
  client?: Client;
  /** Configuration for the app's database. */
  db?: Db;
  /** Configuration for the app's email sender. */
  emailSender?: EmailSender;
  /** Configuration for the app's WebSocket support. */
  webSocket?: WebSocket;
  /**
   * The specification ({@link Spec}) of the app.
   *
   * Build entries with the dedicated constructors ({@link page}, {@link route},
   * {@link query}, {@link action}, {@link api}, {@link apiNamespace},
   * {@link job}, {@link crud}).
   */
  spec: Spec;
}

/**
 * Wasp compiler metadata used by the app.
 */
export interface Wasp {
  /**
   * The Wasp version this app is built for, as an npm-compatible
   * [SemVer range](https://github.com/npm/node-semver#ranges)
   * (e.g. `"^0.24.0"`).
   */
  version: string;
}

/**
 * Authentication configuration and lifecycle hooks.
 *
 * See the [Auth overview](https://wasp.sh/docs/auth/overview) for how the
 * `User` entity is connected to auth. Auth methods (username, email, OAuth)
 * are configured on the auth provider package that implements them.
 * If hooks are async, Wasp awaits them. All hooks receive `prisma` and `req`
 * in their input.
 *
 * In TypeScript, you can type each hook implementation with its matching
 * type from `wasp/server/auth` (e.g. `OnBeforeSignupHook`). See
 * [Auth Hooks](https://wasp.sh/docs/auth/auth-hooks) for the full hook
 * inputs and examples.
 *
 * @example
 * ```ts
 * import { waspAuth } from "@wasp.sh/auth/spec"
 * import { app } from "@wasp.sh/spec"
 *
 * export default app({
 *   // ...
 *   auth: {
 *     userEntity: "User",
 *     providers: [
 *       waspAuth({
 *         methods: {
 *           usernameAndPassword: {}, // use this or email, not both
 *           google: {},
 *           gitHub: {},
 *         },
 *       }),
 *     ],
 *     onAuthFailedRedirectTo: "/login",
 *   },
 * })
 * ```
 */
export interface Auth {
  /**
   * Name of the Prisma model that represents the application user connected to
   * your business logic.
   *
   * The user entity needs to have an ID field that uniquely identifies each
   * user. It can be of any name and type, but it needs to be marked with `@id`:
   *
   * ```prisma title="schema.prisma"
   * model User {
   *   id Int @id @default(autoincrement())
   * }
   * ```
   *
   * You can add any other fields you want to the user entity. Make sure to also
   * define them in the `userSignupFields` field if they need to be set during
   * the sign-up process.
   *
   * See [Accessing User Data](https://wasp.sh/docs/auth/entities) for how the
   * user entity connects to the rest of the auth system.
   *
   * `userEntity` is provider-independent: whichever provider authenticates a
   * request, `context.user` is always a row of this entity.
   */
  userEntity: EntityName;
  /**
   * Route that Wasp redirects unauthenticated users to when they try to
   * access a page that has `authRequired: true`.
   *
   * See [Adding Auth to the Project](https://wasp.sh/docs/tutorial/auth#adding-auth-to-the-project)
   * for an example.
   */
  onAuthFailedRedirectTo: string;
  /**
   * The authentication providers that establish and verify user identity, in
   * order of declaration. Each entry is an independent identity system -- an
   * audience, not a sign-in method: a whole Better Auth instance with all its
   * methods is ONE provider here, and Wasp performs no account linking across
   * providers (the same human signing in through two providers gets two
   * separate rows of your user entity).
   *
   * Every provider is an adapter package, Wasp's own auth included:
   *
   * ```ts
   * import { waspAuth } from "@wasp.sh/auth/spec";
   * import { clerk } from "@wasp.sh/auth-clerk/spec";
   *
   * auth: {
   *   userEntity: "User",
   *   onAuthFailedRedirectTo: "/login",
   *   providers: [waspAuth({ methods: { email: { ... } } }), clerk()],
   * }
   * ```
   *
   * A hand-written adapter goes through {@link customAuthProvider}.
   * Provider-specific configuration (auth methods, method hooks) lives in the
   * package's own spec helper. Provider ids must be pairwise distinct. Wasp
   * still owns everything downstream of authentication: every provider's
   * login ends in the same Wasp session, and `context.user` is always a row
   * in your own user entity, whichever provider vouched for the request
   * (`user.sessionProviderId` says which).
   */
  providers: AuthProviders;
  /**
   * App-level auth lifecycle hooks, fired at Wasp-owned choke points --
   * identity provisioning (`onBeforeSignup` can veto by throwing,
   * `onAfterSignup` observes) and session minting (`onBeforeLogin` can veto,
   * `onAfterLogin` observes) -- for EVERY provider: Wasp's own methods,
   * adapter packages, hand-written providers. A provider can neither forget
   * nor forge them, because they fire where Wasp owns the control flow.
   *
   * Method-specific hooks (`onAfterEmailVerified`, `onBeforeOAuthRedirect`)
   * belong to the auth package that implements the method, and are configured
   * through its spec helper.
   */
  hooks?: AuthLifecycleHooks;
}

/**
 * See {@link Auth.hooks}.
 *
 * @category Auth
 */
export interface AuthLifecycleHooks {
  /**
   * Called before a user is created, whichever provider is signing them up.
   * Throw to reject the signup. Runs before any `userSignupFields` getters.
   *
   * `req` is absent when an adapter provisions eagerly outside a request.
   *
   * @category Hooks
   */
  onBeforeSignup?: Reference<AnyFunction>;
  /**
   * Called after a user is created, whichever provider signed them up.
   * For social auth, receives `oauth` fields including tokens.
   *
   * @category Hooks
   */
  onAfterSignup?: Reference<AnyFunction>;
  /**
   * Called before a Wasp session is minted, whichever provider verified the
   * login. Throw to reject the login.
   *
   * @category Hooks
   */
  onBeforeLogin?: Reference<AnyFunction>;
  /**
   * Called after a Wasp session is minted, whichever provider verified the
   * login. For social auth, receives `oauth` fields including tokens.
   *
   * @category Hooks
   */
  onAfterLogin?: Reference<AnyFunction>;
}

/**
 * The app's auth providers: a non-empty array of {@link AuthProviderConfig}.
 *
 * @category Auth
 */
export type AuthProviders = readonly [
  AuthProviderConfig,
  ...AuthProviderConfig[],
];

/**
 * One authentication provider of the app: a manifest produced by an adapter
 * package's spec helper (`waspAuth()` from `@wasp.sh/auth/spec`, `clerk()`
 * from `@wasp.sh/auth-clerk/spec`, ...) or by {@link customAuthProvider}.
 * Wasp's own auth is an adapter package like any other.
 *
 * @category Auth
 */
export type AuthProviderConfig = AuthProviderManifest;

/**
 * An env var an external auth provider needs. Wasp renders these into the
 * app's generated env validation, so a missing var fails at boot with `doc`
 * as the explanation instead of failing at the first authenticated request.
 *
 * @category Auth
 */
export interface EnvVarRequirement {
  name: string;
  optional?: boolean;
  doc?: string;
  /**
   * Value used in development when the var is unset, so `wasp start` works
   * out of the box; in production the var stays required (unless `optional`).
   * Server-side vars only.
   */
  devDefault?: string;
}

/**
 * Runtime facets an adapter may request from Wasp (the manifest's `uses`
 * list). Mirrors `RuntimeGrantName` in `@wasp.sh/auth-contract`; a closed set,
 * because the generator can only wire facets it knows.
 *
 * @category Experimental
 */
export type AuthRuntimeGrantName =
  | "wasp-sessions"
  | "email-send"
  | "identity-namespaces";

/**
 * EXPERIMENTAL. An external auth provider, described declaratively.
 *
 * Adapter packages produce this from their spec helpers (e.g. `clerk()` from
 * `@wasp.sh/auth-clerk/spec`), and hand-written adapters produce it via
 * {@link customAuthProvider}. Both go through `defineAuthProviderManifest`,
 * which validates the manifest and stamps it as authentic -- the compiler
 * rejects hand-crafted object literals.
 *
 * @category Experimental
 */
export interface AuthProviderManifest {
  /** Discriminates the provider union. Always `"external"`. */
  kind: "external";
  /**
   * Version of the auth provider contract the adapter was built against.
   * Wasp rejects manifests with a contract version it does not support,
   * which turns adapter/compiler version skew into a clear error.
   */
  contractVersion: 1;
  /**
   * Stable identifier of the provider ("wasp", "clerk", "better-auth"). It is
   * the provider's identity namespace: identities Wasp provisions for this
   * provider's subjects are recorded under it (or under `${id}:${suffix}`,
   * see {@link identityNamespaces}), and sessions record it as their minting
   * provider. Must stay stable across deploys and contain no `:`.
   */
  id: string;
  /**
   * The provider's server adapter: either the module specifier of an adapter
   * package's server entry (which must export `createServerAdapter`), or a
   * reference to a user-code module exporting an `AuthProvider`.
   */
  server: { package: string } | Reference<AnyObject>;
  /**
   * Module specifier of an adapter package's client entry (which must export
   * `createClientAdapter`). Wasp instantiates it and wires the client side --
   * context wrapper, credential transport, logout -- automatically.
   */
  client?: { package: string };
  /**
   * Routes the provider wants mounted on Wasp's server, for providers that
   * bring their own HTTP endpoints (Wasp's own auth mounts at `/auth/wasp`,
   * Better Auth at `/better-auth`). Anything under `/auth` is fine except the
   * framework's own `/auth/me`, `/auth/logout` and `/auth/login`. `rawBody`
   * mounts them without the JSON body parser, for handlers that read the body
   * themselves.
   */
  routes?: { basePath: `/${string}`; rawBody?: boolean };
  /**
   * The provider's capabilities, as an open set of strings. Known today:
   * `"issue-sessions"`, `"session-revocation"`, `"cookie-transport"`.
   * Unknown entries are ignored, so adapters can declare capabilities newer
   * than the compiler.
   */
  capabilities: string[];
  /** Env vars the provider needs, rendered into generated env validation. */
  env: { server: EnvVarRequirement[]; client: EnvVarRequirement[] };
  /**
   * Runtime facets the adapter requests from Wasp. A declared grant appears
   * as a member of the adapter's `WaspServerRuntime`; an undeclared one is
   * absent. Declaring `"email-send"` requires the app to configure
   * `emailSender` (a compile error otherwise). Default: none.
   */
  uses?: AuthRuntimeGrantName[];
  /**
   * Identity namespaces this provider records identities under. Defaults to
   * `[id]`. Every extra namespace must be `` `${id}:${suffix}` `` (e.g.
   * `"wasp:email"`, `"better-auth:passkey"`), which makes cross-provider
   * identity collisions impossible by construction, and declaring more than
   * one requires the `"identity-namespaces"` grant in `uses`.
   */
  identityNamespaces?: string[];
  /**
   * Populates the app's user entity when Wasp provisions a local user for a
   * subject it has not seen before, from the claims the provider verified.
   * Required in practice when the user entity has non-nullable fields.
   */
  userSignupFields?: Reference<AnyObject>;
  /**
   * Setup function for the provider's underlying library, following the same
   * convention as `db.prismaSetupFn`: a reference to a user-code function the
   * adapter calls with its integration config, whose return value becomes the
   * configuration to use. This is how an app reaches everything serializable
   * options cannot carry -- hooks, plugins, email-sending callbacks.
   */
  setupFn?: Reference<AnyFunction>;
  /**
   * Any other user code the adapter calls back into (signup field getters,
   * OAuth config functions, email content functions, method-specific hooks),
   * keyed by the name the adapter expects. Each reference reaches the
   * adapter's server factory as `extensions[name]`; the adapter types them
   * precisely, Wasp only forwards them.
   */
  extensions?: Record<string, Reference<AnyFunction | AnyObject>>;
  /**
   * Serializable adapter options, passed verbatim to the adapter's server
   * factory. Must survive a JSON round-trip; the compiler checks.
   */
  options?: unknown;
  /**
   * Marks a manifest as constructed by `defineAuthProviderManifest` rather
   * than hand-crafted. Adapters never set this themselves.
   */
  readonly __waspAuthProviderManifest: true;
}

/**
 * Server-side application configuration.
 *
 * See [Server Config](https://wasp.sh/docs/project/server-config) for usage
 * details.
 *
 * @example
 * ```ts
 * import { app } from "@wasp.sh/spec"
 * import { myMiddlewareConfigFn, mySetupFunction } from "./src/myServerSetupCode" with { type: "ref" }
 *
 * export default app({
 *   // ...
 *   server: {
 *     setupFn: mySetupFunction,
 *     middlewareConfigFn: myMiddlewareConfigFn,
 *   },
 * })
 * ```
 */
export interface Server {
  /**
   * Async function called once on server start. Wasp awaits it before the
   * server accepts requests. Receives a context containing the Express `app`
   * and underlying `http.Server`, so you can register custom routes, set up
   * additional databases or WebSockets, or kick off scheduled jobs.
   *
   * In TypeScript, you can type the function with the `ServerSetupFn` type
   * from `wasp/server`.
   *
   * @example
   * ```ts
   * import { type ServerSetupFn } from "wasp/server"
   *
   * export const mySetupFunction: ServerSetupFn = async () => {
   *   await setUpSomeResource()
   * }
   * ```
   */
  setupFn?: Reference<AnyFunction>;
  /**
   * Function that customizes the global Express middleware stack. Affects
   * all operations and APIs.
   *
   * See [Configuring Middleware](https://wasp.sh/docs/advanced/middleware-config).
   */
  middlewareConfigFn?: Reference<AnyFunction>;
  /**
   * Zod schema used to validate user-defined server environment variables on
   * startup. Wasp merges it with built-in validation for Wasp-defined env vars
   * when validating `process.env`. Define the schema with
   * `defineEnvValidationSchema` from `wasp/env` to make sure it is
   * type-checked.
   *
   * {@include ./referenceImports.md}
   *
   * For example:
   *
   * ```ts
   * import { serverEnvSchema } from './src/env' with { type: 'ref' }
   * ```
   *
   * See [Env Vars](https://wasp.sh/docs/project/env-vars).
   */
  envValidationSchema?: Reference<ZodSchema>;
}

/**
 * Client-side application configuration.
 *
 * See [Client Config](https://wasp.sh/docs/project/client-config) for usage
 * details.
 *
 * @example
 * ```ts
 * import { app } from "@wasp.sh/spec"
 * import Root from "./src/Root" with { type: "ref" }
 * import mySetupFunction from "./src/myClientSetupCode" with { type: "ref" }
 *
 * export default app({
 *   // ...
 *   client: {
 *     rootComponent: Root,
 *     setupFn: mySetupFunction,
 *     baseDir: "/my-app",
 *   },
 * })
 * ```
 */
export interface Client {
  /**
   * Wrapper React component rendered at the root of the client app. Must
   * render `Outlet` from `react-router` to show the current page.
   *
   * Useful for layouts, providers (Redux, theme, etc.), and global UI.
   *
   * @example
   * ```tsx
   * import { Outlet } from "react-router"
   * import store from "./store"
   * import { Provider } from "react-redux"
   *
   * export default function Root() {
   *   return (
   *     <Provider store={store}>
   *       <Layout />
   *     </Provider>
   *   )
   * }
   *
   * function Layout() {
   *   return (
   *     <div>
   *       <header>
   *         <h1>My App</h1>
   *       </header>
   *
   *       {/* The current page will be rendered here *\/}
   *       <Outlet />
   *
   *       <footer>
   *         <p>My App footer</p>
   *       </footer>
   *     </div>
   *   )
   * }
   * ```
   */
  rootComponent?: Reference<AnyFunction>;
  /**
   * Async function Wasp calls once while initializing the client app. Wasp
   * awaits it before rendering the app. It receives no arguments, and its
   * return value is ignored. Use it for one-time client-side setup, such as
   * configuring the query client or starting client-side periodic jobs.
   *
   * @example
   * ```ts
   * export default async function mySetupFunction(): Promise<void> {
   *   // Run some code
   * }
   * ```
   */
  setupFn?: Reference<AnyFunction>;
  /**
   *
   * If you need to serve the client from a subdirectory, you can use the
   * `baseDir` option.
   *
   * If you set `baseDir` to `/my-app` for example, that will make Wasp set the
   * `basename` prop of the `Router` to `/my-app`. It will also set the `base`
   * option of the Vite config to `/my-app`.
   *
   * This means that if you serve your app from `https://example.com/my-app`,
   * the router will work correctly, and all the assets will be served from
   * `https://example.com/my-app`.
   *
   * Check the [Base directory
   * docs](https://wasp.sh/docs/project/client-config#base-directory) for
   * important details.
   */
  baseDir?: `/${string}`;
  /**
   * Zod schema used to validate user-defined client environment variables at
   * build time. Wasp merges it with built-in validation for Wasp-defined env
   * vars when validating `import.meta.env`. Client env vars must be prefixed
   * with `REACT_APP_`. Define the schema with `defineEnvValidationSchema`
   * from `wasp/env` to make sure it is type-checked.
   *
   * {@include ./referenceImports.md}
   *
   * For example:
   *
   * ```ts
   * import { clientEnvSchema } from './src/env' with { type: 'ref' }
   * ```
   *
   * See [Env Vars](https://wasp.sh/docs/project/env-vars).
   */
  envValidationSchema?: Reference<ZodSchema>;
}

/**
 * Database configuration.
 *
 * See [Databases](https://wasp.sh/docs/data-model/databases) for seeding and
 * Prisma client customization.
 *
 * @example
 * ```ts
 * import { app } from "@wasp.sh/spec"
 * import devSeed from "./src/dbSeeds" with { type: "ref" }
 * import { setUpPrisma } from "./src/prisma" with { type: "ref" }
 *
 * export default app({
 *   // ...
 *   db: {
 *     seeds: [devSeed],
 *     prismaSetupFn: setUpPrisma,
 *   },
 * })
 * ```
 */
export interface Db {
  /**
   * Async functions runnable with `wasp db seed [name]` to populate the
   * database with initial data. Each function receives Wasp's Prisma Client;
   * the name passed to `wasp db seed` matches the function's identifier in the
   * import.
   *
   * See [Seeding the Database](https://wasp.sh/docs/data-model/databases#seeding-the-database).
   */
  seeds?: Reference<AnyFunction>[];
  /**
   * Function that sets up and returns a configured Prisma Client instance.
   * Use this to add Prisma logging or client extensions.
   *
   * See Prisma's [logging](https://www.prisma.io/docs/orm/prisma-client/observability-and-logging/logging)
   * and [client extensions](https://www.prisma.io/docs/orm/prisma-client/client-extensions)
   * docs.
   *
   * @example
   * ```ts
   * import { PrismaClient } from "@prisma/client"
   *
   * export const setUpPrisma = () => {
   *   const prisma = new PrismaClient({
   *     log: ["query", "info", "warn", "error"],
   *   })
   *
   *   return prisma
   * }
   * ```
   */
  prismaSetupFn?: Reference<AnyFunction>;
}

/**
 * Email sender configuration.
 *
 * Required for the email auth flows (verification, password reset) and
 * available for sending arbitrary emails via `wasp/server/email`.
 *
 * See [Sending Emails](https://wasp.sh/docs/advanced/email).
 *
 * @example
 * ```ts
 * import { app } from "@wasp.sh/spec"
 *
 * export default app({
 *   // ...
 *   emailSender: {
 *     provider: "SMTP",
 *     defaultFrom: {
 *       name: "Example",
 *       email: "hello@itsme.com",
 *     },
 *   },
 * })
 * ```
 */
export interface EmailSender {
  /** Provider Wasp uses to deliver outgoing emails. */
  provider: EmailSenderProviderName;
  /**
   * The default sender's details. If you set this field, you don't need to
   * provide the `from` field when sending an email.
   */
  defaultFrom?: EmailFromField;
}

/**
 * Supported email sender providers.
 *
 * `"Dummy"` logs emails to the console instead of sending them, which is
 * convenient during development, but not allowed in production.
 */
export type EmailSenderProviderName =
  | "SMTP"
  | "SendGrid"
  | "Mailgun"
  | "Resend"
  | "Dummy";

/**
 * Sender identity used in outgoing emails.
 */
export interface EmailFromField {
  /** Optional display name shown alongside the email address. */
  name?: string;
  /** Email address the message is sent from. */
  email: string;
}

/**
 * WebSocket configuration.
 *
 * See [Web Sockets](https://wasp.sh/docs/advanced/web-sockets) for handler
 * shape and client-side usage.
 *
 * @example
 * ```ts
 * import { app } from "@wasp.sh/spec"
 * import { webSocketFn } from "./src/webSocket" with { type: "ref" }
 *
 * export default app({
 *   // ...
 *   webSocket: {
 *     fn: webSocketFn,
 *     autoConnect: true,
 *   },
 * })
 * ```
 */
export interface WebSocket {
  /**
   * Function that registers Socket.IO event handlers. Wasp calls it once on
   * server start with the Socket.IO server instance and a context containing
   * all app entities. If a connected socket is authenticated, Wasp stores the
   * user on `socket.data.user`.
   *
   * See [the `websocketFn` docs](https://wasp.sh/docs/advanced/web-sockets#websocketfn).
   */
  fn: Reference<AnyFunction>;
  /**
   * If `true` (the default), the client connects to the WebSocket server as
   * soon as the app loads. Set to `false` to connect manually via
   * `useSocket`.
   *
   * @default true
   */
  autoConnect?: boolean;
}

/**
 * A single {@link SpecElement}, or an (optionally nested) array of them, that
 * makes up the {@link App.spec}.
 *
 * This lets you define related elements separately (for example, one array per
 * feature) and compose them together. The nested structure is treated as a single
 * flat list of elements.
 *
 * @category Specifications
 *
 * @example
 * Nested arrays let you compose a spec from separate sub-specs:
 * ```ts
 * import { app } from "@wasp.sh/spec"
 *
 * const authSpec: Spec = [signupRoute, loginRoute];
 * const tasksSpec: Spec = [tasksRoute, getTasks, createTask];
 *
 * export default app({
 *   // ...
 *   spec: [authSpec, tasksSpec],
 * });
 * ```
 */
export type Spec = SpecElement | Spec[];

/**
 * Union of every kind of specification element that can appear in {@link App.spec}.
 *
 * Each variant is produced by its matching constructor.
 *
 * @category Specifications
 */
export type SpecElement =
  | Page
  | Route
  | Query
  | Action
  | Api
  | ApiNamespace
  | Job
  | Crud;

/**
 * A page in the app, normally a React component rendered for a {@link Route}.
 *
 * Create one with the {@link page} constructor.
 *
 * @category Specifications
 */
export interface Page extends BaseSpecElement<"page"> {
  /** React component rendered for this page. */
  component: Reference<AnyFunction>;
  /**
   * If `true`, only authenticated users can access this page, whichever
   * provider they logged in through. Unauthenticated visitors are redirected
   * to {@link Auth.onAuthFailedRedirectTo}.
   *
   * A list of auth provider ids (`["wasp"]`, `["external:clerk"]`, ...)
   * additionally requires the session to have been minted by one of the
   * listed providers: unauthenticated visitors are still redirected, but a
   * user logged in through a non-listed provider sees an access-denied page
   * instead (redirecting them to login would loop). Page-level checks are UX;
   * the operations the page calls carry the real enforcement via
   * {@link Query.auth}/{@link Action.auth}.
   *
   * Cannot be combined with {@link Route.prerender}.
   *
   * @default false
   */
  authRequired?: AuthRequirement;
}

/**
 * Whether (and through which providers) a page or operation requires
 * authentication: `false` = no auth, `true` = any valid session, a non-empty
 * list of auth provider ids = only sessions minted by one of the listed
 * providers. Ids are checked against `app.auth.providers` at build time.
 *
 * @category Auth
 */
export type AuthRequirement =
  | boolean
  | readonly [AuthProviderIdRef, ...AuthProviderIdRef[]];

/**
 * An auth provider id, as recorded on sessions and identities: `"wasp"` for
 * Wasp's own auth, an external provider's manifest id (`"external:clerk"`)
 * otherwise. Validated against the app's configured providers at build time.
 *
 * @category Auth
 */
export type AuthProviderIdRef = string;

/**
 * A URL path mapped to a {@link Page}.
 *
 * Create one with the {@link route} constructor.
 *
 * See [Routing](https://wasp.sh/docs/advanced/routing) for path patterns
 * (dynamic segments, optional segments, splats).
 *
 * @category Specifications
 */
export interface Route extends BaseSpecElement<"route"> {
  /** Unique route name. */
  name: string;
  /**
   * URL path. Supports React Router patterns like `/tasks/:id`,
   * `/photo/:id/edit?`, and `/files/*`.
   */
  path: string;
  /** Page rendered when this route matches. */
  page: Page;
  /**
   * Render this route to static HTML at build time. Useful for SEO and AI
   * crawlers. The page then hydrates on the client for full interactivity.
   *
   * Accepts either:
   * - `true` — prerender this route's own path. The path must be fully static
   *   (no `:paramName`, `*`, or `?` segments).
   * - an array of concrete paths to prerender. Use this to prerender specific
   *   instances of a dynamic route (e.g. `["/blog/intro", "/blog/changelog"]`
   *   for a `"/blog/:slug"` route). Every listed path must be fully static and
   *   must match this route's path pattern.
   *
   * In either case the route's page cannot have {@link Page.authRequired} set,
   * since prerendered content can't depend on the logged-in user. See
   * [Prerendering](https://wasp.sh/docs/advanced/prerendering).
   *
   * @default false
   *
   * @example
   * ```ts
   * import { app, page, route } from "@wasp.sh/spec"
   * import { LandingPage } from "./src/LandingPage" with { type: "ref" }
   * import { BlogPostPage } from "./src/BlogPostPage" with { type: "ref" }
   *
   * export default app({
   *   // ...
   *   spec: [
   *     route("LandingRoute", "/", page(LandingPage), {
   *       prerender: true,
   *     }),
   *     route("BlogPostRoute", "/blog/:slug", page(BlogPostPage), {
   *       prerender: ["/blog/intro", "/blog/changelog"],
   *     }),
   *   ],
   * })
   * ```
   */
  prerender?: boolean | readonly string[];
  /**
   * Lazy-load the page's component.
   *
   * Set to `false` to include the page in the initial client bundle, which
   * avoids the brief loading delay on first navigation at the cost of a larger
   * initial download.
   *
   * @default true
   */
  lazy?: boolean;
}

/**
 * A server-side read-only operation, callable from the client and the server.
 *
 * Create one with the {@link query} constructor.
 *
 * See [Queries](https://wasp.sh/docs/data-model/operations/queries).
 *
 * @category Specifications
 */
export interface Query extends BaseSpecElement<"query"> {
  /**
   * Reference to the Query's NodeJS implementation.
   *
   * See [the
   * docs](https://wasp.sh/docs/data-model/operations/queries#implementing-queries)
   * for details on the implementation and its context.
   */
  fn: Reference<AnyFunction>;
  /**
   * A list of entities you wish to use inside your Query.
   *
   * See [Using Entities in Queries](https://wasp.sh/docs/data-model/operations/queries#using-entities-in-queries).
   */
  entities?: EntityName[];
  /**
   * Whether this Query requires auth. If your app has auth enabled, this
   * defaults to `true`.
   *
   * `true` attaches `context.user` (possibly null -- the Query decides); a
   * list of auth provider ids is self-enforcing: no session is a 401, and a
   * session minted by a non-listed provider is a 403.
   *
   * Only available if your app has auth enabled.
   */
  auth?: AuthRequirement;
}

/**
 * A server-side write operation, callable from the client and the server.
 *
 * Create one with the {@link action} constructor.
 *
 * See [Actions](https://wasp.sh/docs/data-model/operations/actions).
 *
 * @category Specifications
 */
export interface Action extends BaseSpecElement<"action"> {
  /**
   * Reference to the Action's NodeJS implementation.
   *
   * See [the
   * docs](https://wasp.sh/docs/data-model/operations/actions#implementing-actions)
   * for details on the implementation and its context.
   */
  fn: Reference<AnyFunction>;
  /**
   * A list of entities you wish to use inside your Action.
   *
   * See [Using Entities in Actions](https://wasp.sh/docs/data-model/operations/actions#using-entities-in-actions).
   */
  entities?: EntityName[];
  /**
   * Whether this Action requires auth. If your app has auth enabled, this
   * defaults to `true`.
   *
   * `true` attaches `context.user` (possibly null -- the Action decides); a
   * list of auth provider ids is self-enforcing: no session is a 401, and a
   * session minted by a non-listed provider is a 403.
   *
   * Only available if your app has auth enabled.
   */
  auth?: AuthRequirement;
}

/**
 * A custom HTTP endpoint mounted on the Wasp server.
 *
 * Create one with the {@link api} constructor.
 *
 * Unlike operations, APIs are plain Express handlers that receive `req`,
 * `res`, and a Wasp `context`. They are useful for webhooks and any
 * non-standard HTTP interaction.
 *
 * See [Custom HTTP API Endpoints](https://wasp.sh/docs/advanced/apis).
 *
 * @category Specifications
 */
export interface Api extends BaseSpecElement<"api"> {
  /** HTTP method this endpoint responds to. */
  method: HttpMethod;
  /** Express path of the endpoint (e.g. `"/webhooks/stripe"`). */
  path: string;
  /** Reference to the API's NodeJS implementation. */
  fn: Reference<AnyFunction>;
  /**
   * Reference to an Express middleware config function for this endpoint only.
   *
   * See [Configuring API middleware](https://wasp.sh/docs/advanced/middleware-config#2-customize-api-specific-middleware).
   */
  middlewareConfigFn?: Reference<AnyFunction>;
  /**
   * Entities the handler operates on. Wasp injects a Prisma delegate for
   * each one into the handler's `context.entities`.
   *
   * See [Using Entities in APIs](https://wasp.sh/docs/advanced/apis#using-entities-in-apis).
   */
  entities?: EntityName[];
  /**
   * If `true`, the handler requires the request to come from an
   * authenticated user and receives `context.user`. Defaults to `true` when
   * the app has auth enabled, and `false` otherwise. Set to `false` to skip
   * parsing the JWT from the Authorization header.
   *
   * A list of auth provider ids is self-enforcing: no session is a 401, and
   * a session minted by a non-listed provider is a 403.
   */
  auth?: AuthRequirement;
}

/**
 * Shared middleware applied to every {@link Api} mounted under a path
 * prefix.
 *
 * Create one with the {@link apiNamespace} constructor.
 *
 * @category Specifications
 */
export interface ApiNamespace extends BaseSpecElement<"apiNamespace"> {
  /** Reference to an Express middleware config function for this namespace. */
  middlewareConfigFn: Reference<AnyFunction>;
  /** Path prefix the namespace applies to (e.g. `"/webhooks"`). */
  path: string;
}

/**
 * HTTP methods supported by an {@link Api}.
 *
 * Use `"ALL"` to match any method.
 */
export type HttpMethod = "ALL" | "GET" | "POST" | "PUT" | "DELETE";

/**
 * A background job. Can be submitted ad-hoc or run on a recurring schedule.
 *
 * Create one with the {@link job} constructor.
 *
 * See [Recurring Jobs](https://wasp.sh/docs/advanced/jobs).
 *
 * @category Specifications
 */
export interface Job extends BaseSpecElement<"job"> {
  /**
   * Reference to the job's NodeJS implementation. It receives the submitted
   * args and a context containing the declared entities.
   *
   * See [Jobs documentation](https://wasp.sh/docs/advanced/jobs#worker-api) for more details.
   */
  fn: Reference<AnyFunction>;
  /**
   * Executor backing this job.
   *
   * Currently only `"PgBoss"` is available.
   */
  executor: JobExecutor;
  /** Cron schedule that runs the job automatically. */
  schedule?: Schedule;
  /**
   * Entities the worker operates on. Wasp injects a Prisma delegate for
   * each one into the worker's `context.entities`.
   *
   * This works like entity access in queries and actions. See
   * [Using Entities in Queries](https://wasp.sh/docs/data-model/operations/queries#using-entities-in-queries).
   */
  entities?: EntityName[];
  /**
   * Executor-specific default options used when submitting the job.
   *
   * These options are passed through to the executor and can be overridden
   * when submitting the job or by {@link Schedule.executorOptions} for
   * scheduled runs. For example, with PgBoss this can set retry limits,
   * expiration, or priority.
   *
   * @default {}
   */
  performExecutorOptions?: ExecutorOptions;
}

/**
 * Supported job executors.
 *
 * Currently only `"PgBoss"` is available, which stores jobs in the app's
 * PostgreSQL database via [pg-boss](https://github.com/timgit/pg-boss).
 */
export type JobExecutor = "PgBoss";

/**
 * Cron schedule for a {@link Job}.
 */
export interface Schedule {
  /**
   * Five-field cron expression (e.g. `"0 * * * *"` for hourly).
   *
   * Wasp supports minute-level precision.
   *
   * See pg-boss's [scheduling docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#scheduling)
   * for the rationale. Use [Crontab Guru](https://crontab.guru/#0_*_*_*_*)
   * to build cron expressions.
   */
  cron: string;
  /** Arguments passed to the worker function on each scheduled run. */
  args?: object;
  /**
   * Executor-specific options applied only to scheduled runs. These override
   * or extend {@link Job.performExecutorOptions}.
   */
  executorOptions?: ExecutorOptions;
}

/**
 * Executor-specific options.
 *
 * The `pgBoss` field is forwarded as-is to the matching pg-boss API. See
 * the [pg-boss send options docs](https://github.com/timgit/pg-boss/blob/8.4.2/docs/readme.md#sendname-data-options)
 * for the supported keys.
 */
export interface ExecutorOptions {
  pgBoss: object;
}

/**
 * Auto-generated CRUD operations for an entity.
 *
 * Create one with the {@link crud} constructor.
 *
 * See [Automatic CRUD](https://wasp.sh/docs/data-model/crud).
 *
 * @category Specifications
 */
export interface Crud extends BaseSpecElement<"crud"> {
  /** Unique name for this CRUD. */
  name: string;
  /** Entity to generate operations for. */
  entity: EntityName;
  /** Which operations to generate and how to configure each one. */
  operations: CrudOperations;
}

/**
 * Mapping of CRUD operations to their options.
 *
 * Each key enables the matching operation; an empty object enables it with
 * Wasp's defaults.
 *
 * The default implementations map directly to Prisma calls on the entity, and
 * use the field marked with `@id` in the Prisma schema as the entity ID. Unless
 * an operation is marked with {@link CrudOperationOptions.isPublic}, Wasp
 * checks that an authenticated user is making the request.
 *
 * CRUD operations are implemented with Wasp queries and actions. See
 * [Operations](https://wasp.sh/docs/data-model/operations/overview).
 */
export interface CrudOperations {
  /**
   * @default
   * `Entity.findUnique({ where: { id: args.id } })`
   */
  get?: CrudOperationOptions;
  /**
   * @default
   * `Entity.findMany()`
   */
  getAll?: CrudOperationOptions;
  /**
   * @default
   * `Entity.create({ data: args.data })`
   */
  create?: CrudOperationOptions;
  /**
   * @default
   * `Entity.update({ where: { id: args.id }, data: args.data })`
   */
  update?: CrudOperationOptions;
  /**
   * @default
   * `Entity.delete({ where: { id: args.id } })`
   */
  delete?: CrudOperationOptions;
}

/**
 * Configuration for a single CRUD operation.
 */
export interface CrudOperationOptions {
  /**
   * If `true`, the operation skips Wasp's auth check and can be called by
   * anyone.
   *
   * @default false
   */
  isPublic?: boolean;
  /**
   * Reference to a custom implementation that replaces Wasp's auto-generated
   * one. Use this when you need custom business logic for an operation (e.g.
   * attaching `userId` on `create`).
   *
   * See the [CRUD guide](https://wasp.sh/docs/data-model/crud#adding-crud-to-the-task-entity-)
   * for an override example.
   *
   * The override receives the caller-provided `args` payload and a
   * Wasp-provided `context` containing the current user and the entity being
   * operated on.
   *
   * **Important**
   * Use an override to validate or filter input before saving it;
   * the default `create` and `update` implementations pass client-sent data to
   * Prisma.
   */
  overrideFn?: Reference<AnyFunction>;
}

/**
 * One of the app's entity names, matching a model in `schema.prisma`.
 */
export type EntityName = keyof Entities extends never
  ? string
  : Extract<keyof Entities, string>;

type Entities = FromRegister<"entities", {}>;

/**
 * Represents a value from your app passed into the Wasp spec.
 *
 * {@include ./referenceImports.md}
 *
 * @category References
 *
 * @typeParam AppValue - The expected TypeScript type for the value from your
 * app. The field using `Reference<AppValue>` decides this type. For example,
 * {@link Page.component} expects a React component, {@link Query.fn} expects a
 * server function, and env schema fields expect a {@link ZodSchema}.
 *
 * @example
 * ```ts
 * import { app, query } from "@wasp.sh/spec"
 * import { getTasks } from "./src/queries" with { type: "ref" }
 *
 * export default app({
 *   // ...
 *   spec: [
 *     query(getTasks, { entities: ["Task"] }),
 *   ],
 * })
 * ```
 */
export type Reference<AppValue> = RefObject | AppValue;

export type {
  /**
   * Default-import variant of {@link RefObjectDescriptor}.
   */
  DefaultRefObjectDescriptor,
  /**
   * Named-import variant of {@link RefObjectDescriptor}.
   */
  NamedRefObjectDescriptor,
  /**
   * A user-facing reference object, pointing to a value in the project's `src`
   * directory.
   */
  RefObject,
  /**
   * Descriptor accepted by {@link ref}.
   */
  RefObjectDescriptor,
} from "../refObject.js";

interface BaseSpecElement<Kind extends string> {
  /**
   * The internal Wasp type of a {@link SpecElement}. Used by the compiler.
   * You should not set this field directly, instead use the dedicated constructors.
   */
  kind: Kind;
}

/**
 * Structural type for a runtime Zod schema accepted by Wasp env validation fields.
 *
 * {@include ./referenceImports.md}
 *
 * See {@link Server.envValidationSchema} and
 * {@link Client.envValidationSchema} for examples.
 *
 * To avoid depending on the `zod` package, Wasp structurally recognizes Zod 4
 * schemas via their documented library-author marker. See
 * https://zod.dev/library-authors.
 *
 * @inline
 *
 * @category References
 */
export type ZodSchema = {
  readonly _zod: {
    readonly def: object;
  };
};
