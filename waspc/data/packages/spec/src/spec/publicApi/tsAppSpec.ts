/* eslint-disable @typescript-eslint/no-empty-object-type */
import type { RequireOneOrNone } from "type-fest";
import type { AnyFunction, AnyObject } from "../../typeUtils.js";
import type { RefObject } from "../refObject.js";
import { FromRegister } from "./register.js";

/**
 * Root shape of a Wasp app specification.
 *
 * Pass an `App` to the {@link app} constructor and `export default` the
 * result from `main.wasp.ts`.
 *
 * @category Spec
 *
 * @example
 * ```ts
 * import { app } from '@wasp.sh/spec'
 *
 * export default app({
 *   name: 'todoApp',
 *   title: 'ToDo App',
 *   wasp: { version: '^0.24.0' },
 *   parts: [],
 * })
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
   * Due to a React bug, avoid `defer` on `<script>` tags because it can cause
   * hydration warnings. Use `async` instead.
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
   * All the {@link Part}s of the app.
   *
   * Build entries with the dedicated constructors ({@link page}, {@link route},
   * {@link query}, {@link action}, {@link api}, {@link apiNamespace},
   * {@link job}, {@link crud}).
   */
  parts: Part[];
}

/**
 * Wasp compiler metadata used by the app.
 */
export interface Wasp {
  /**
   * The Wasp version this app is built for, as an npm-compatible version range
   * (e.g. `"^0.24.0"`).
   */
  version: string;
}

/**
 * Authentication configuration and lifecycle hooks.
 *
 * See the [Auth overview](https://wasp.sh/docs/auth/overview) for the
 * supported auth methods and how the `User` entity is connected to auth.
 * If hooks are async, Wasp awaits them. All hooks receive `prisma` and `req`
 * in their input. Hook return values are ignored except for
 * {@link AuthHooks.onBeforeOAuthRedirect}, which can change the redirect URL.
 */
export interface Auth extends AuthHooks {
  /**
   * Name of the Prisma model that represents the application user connected
   * to your business logic.
   *
   * The model must be defined in `schema.prisma` and have an `@id` field. The
   * ID can use any Prisma-supported ID type.
   */
  userEntity: EntityName;
  /** Enabled authentication methods. */
  methods: AuthMethods;
  /**
   * Route that Wasp redirects unauthenticated users to when they try to
   * access a page that has `authRequired: true`.
   */
  onAuthFailedRedirectTo: string;
  /**
   * Route that Wasp redirects users to after a successful login or signup.
   *
   * Only takes effect when using Wasp's built-in Auth UI.
   *
   * @default "/"
   */
  onAuthSucceededRedirectTo?: string;
}

interface AuthHooks {
  /**
   * Called before the user is created. Receives `providerId` plus the common
   * hook input. Throw from this hook to reject a signup based on custom
   * criteria.
   */
  onBeforeSignup?: Reference<AnyFunction>;
  /**
   * Called after the user is created. Receives `providerId`, the created
   * `user`, and, for social auth, `oauth` fields including tokens and the
   * unique OAuth request ID.
   */
  onAfterSignup?: Reference<AnyFunction>;
  /** Called once, after the user verifies their email. Receives `email` and `user`. */
  onAfterEmailVerified?: Reference<AnyFunction>;
  /**
   * Called before redirecting the user to the OAuth provider. Receives the
   * generated `url` and `oauth.uniqueRequestId`. Return `{ url }` to override
   * the redirect URL.
   */
  onBeforeOAuthRedirect?: Reference<AnyFunction>;
  /**
   * Called before the user is logged in. Receives `providerId` and `user`.
   * Throw from this hook to reject a login based on custom criteria.
   */
  onBeforeLogin?: Reference<AnyFunction>;
  /**
   * Called after a successful login. Receives `providerId`, `user`, and, for
   * social auth, `oauth` fields including tokens and the unique OAuth request
   * ID.
   */
  onAfterLogin?: Reference<AnyFunction>;
}

/**
 * Enabled authentication methods for the app.
 *
 * At most one local auth method ({@link LocalAuthMethods}) can be used, but you
 * can enable as many external auth methods ({@link ExternalAuthMethods}) as you
 * need.
 */
export type AuthMethods = RequireOneOrNone<LocalAuthMethods> &
  ExternalAuthMethods;

/**
 * Built-in local auth methods.
 *
 * Pick at most one of these.
 */
export interface LocalAuthMethods {
  /** Enables username and password auth. */
  usernameAndPassword: UsernameAndPasswordConfig;
  /**
   * Enables email and password auth with email verification and password
   * reset flows.
   */
  email: EmailAuthConfig;
}

/**
 * Social (OAuth) auth methods.
 *
 * Each enabled provider also requires the matching client ID and secret to
 * be set as environment variables. See the
 * [Social Auth overview](https://wasp.sh/docs/auth/social-auth/overview) for
 * details on each provider.
 */
export interface ExternalAuthMethods
  extends Partial<Record<SocialAuthMethodName, SocialAuthConfig>> {}

/**
 * Supported social auth providers.
 */
type SocialAuthMethodName =
  | "discord"
  | "google"
  | "gitHub"
  | "keycloak"
  | "microsoft"
  | "slack";

/**
 * Username and password auth configuration.
 */
export interface UsernameAndPasswordConfig extends BaseAuthMethodConfig {}

/**
 * Social auth provider configuration.
 *
 * By default, Wasp stores only the user's provider-specific ID. Use
 * `userSignupFields` to save provider account details or custom signup state
 * on your {@link Auth.userEntity}. Each provider has provider-specific
 * `userSignupFields` and `configFn` behavior, documented in the
 * [Social Auth docs](https://wasp.sh/docs/auth/social-auth/overview).
 */
export interface SocialAuthConfig extends BaseAuthMethodConfig {
  /**
   * Function that returns provider-specific OAuth options (scopes, prompt,
   * etc.). Use this to request extra scopes, such as requesting email/profile
   * data from providers that do not include it by default, or to customize the
   * OAuth flow.
   */
  configFn?: Reference<AnyFunction>;
}

/**
 * Email auth configuration.
 *
 * See the [Email auth docs](https://wasp.sh/docs/auth/email) for the full
 * signup, verification, and password reset flows.
 */
export interface EmailAuthConfig extends BaseAuthMethodConfig {
  /** Sender identity used for verification and password reset emails. */
  fromField: EmailFromField;
  /**
   * Email verification flow configuration.
   *
   * Its `clientRoute` should handle the verification link sent to the user.
   */
  emailVerification: EmailFlowConfig;
  /**
   * Password reset flow configuration.
   *
   * Its `clientRoute` should handle the password reset link and the new
   * password submitted by the user.
   */
  passwordReset: EmailFlowConfig;
}

interface BaseAuthMethodConfig {
  /**
   * Object that defines extra fields to save on the user during signup
   * (e.g. `firstName`, `address`). Each field name must exist on the
   * configured {@link Auth.userEntity}.
   *
   * Each field function receives the data sent from the client and returns
   * the value Wasp saves to the database. For social auth, this data includes
   * provider-specific profile information. If the value is invalid, throw an
   * error. The `password` field is excluded from this object and handled by
   * Wasp's auth backend.
   *
   * See [Signup Fields Customization](https://wasp.sh/docs/auth/overview#signup-fields-customization).
   */
  userSignupFields?: Reference<AnyObject>;
}

/**
 * Configuration for an email-based auth flow (verification or password reset).
 */
export interface EmailFlowConfig {
  /**
   * Function that returns the email content (subject, html, text) Wasp sends
   * for this flow. Verification email functions receive
   * `{ verificationLink }`, and password reset email functions receive
   * `{ passwordResetLink }`. If omitted, Wasp uses a built-in default
   * template.
   */
  getEmailContentFn?: Reference<AnyFunction>;
  /**
   * Name of the route that handles the link sent in the email
   * (e.g. `"EmailVerificationRoute"` or `"PasswordResetRoute"`).
   *
   * The route must be defined in {@link App.parts} with the {@link route}
   * constructor.
   */
  clientRoute: string;
}

/**
 * Server-side application configuration.
 *
 * See [Server Config](https://wasp.sh/docs/project/server-config) for usage
 * details.
 */
export interface Server {
  /**
   * Async function called once on server start. Wasp awaits it before the
   * server accepts requests. Receives a context containing the Express `app`
   * and underlying `http.Server`, so you can register custom routes, set up
   * additional databases or WebSockets, or kick off scheduled jobs.
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
   * when validating `process.env`. Import the schema from app code with a
   * reference import or pass a {@link RefObject}.
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
 */
export interface Client {
  /**
   * Wrapper React component rendered at the root of the client app. Must
   * render `Outlet` from `react-router` to show the current page.
   *
   * Useful for layouts, providers (Redux, theme, etc.), and global UI.
   */
  rootComponent?: Reference<AnyFunction>;
  /**
   * Async function Wasp calls once while initializing the client app. Wasp
   * awaits it before rendering the app. It receives no arguments, and its
   * return value is ignored. Use it for one-time client-side setup, such as
   * configuring the query client or starting client-side periodic jobs.
   */
  setupFn?: Reference<AnyFunction>;
  /**
   * Subpath the client app is served from (e.g. `"/my-app"`).
   *
   * When set, Wasp configures React Router's `basename` for routing and
   * Vite's `base` option for asset URLs, so the app works from
   * `https://example.com/my-app`.
   */
  baseDir?: `/${string}`;
  /**
   * Zod schema used to validate user-defined client environment variables at
   * build time. Wasp merges it with built-in validation for Wasp-defined env
   * vars when validating `import.meta.env`. Import the schema from app code
   * with a reference import or pass a {@link RefObject}. Client env
   * vars must be prefixed with `REACT_APP_`.
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
 */
export interface Db {
  /**
   * Async functions runnable with `wasp db seed [name]` to populate the
   * database with initial data. Each function receives Wasp's Prisma Client;
   * the name passed to `wasp db seed` matches the function's identifier in the
   * import.
   */
  seeds?: Reference<AnyFunction>[];
  /**
   * Function that sets up and returns a configured Prisma Client instance.
   * Use this to add Prisma logging or client extensions.
   */
  prismaSetupFn?: Reference<AnyFunction>;
}

/**
 * Email sender configuration.
 *
 * Required for the email auth flows (verification, password reset) and
 * available for sending arbitrary emails via `wasp/server/email`.
 */
export interface EmailSender {
  /** Provider Wasp uses to deliver outgoing emails. */
  provider: EmailSenderProviderName;
  /**
   * Sender identity used when an email is sent without an explicit `from`.
   */
  defaultFrom?: EmailFromField;
}

/**
 * Supported email sender providers.
 *
 * `"Dummy"` logs emails to the console instead of sending them, which is
 * convenient during development, but not allowed in production.
 */
export type EmailSenderProviderName = "SMTP" | "SendGrid" | "Mailgun" | "Dummy";

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
 */
export interface WebSocket {
  /**
   * Function that registers Socket.IO event handlers. Wasp calls it once on
   * server start with the Socket.IO server instance and a context containing
   * all app entities. If a connected socket is authenticated, Wasp stores the
   * user on `socket.data.user`.
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
 * Union of every kind of app part that can appear in {@link App.parts}.
 *
 * Each variant is produced by its matching constructor.
 */
export type Part =
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
 * @category Parts
 */
export interface Page extends BasePart<"page"> {
  /** React component rendered for this page. */
  component: Reference<AnyFunction>;
  /**
   * If `true`, only authenticated users can access this page. Unauthenticated
   * visitors are redirected to {@link Auth.onAuthFailedRedirectTo}.
   *
   * Cannot be combined with {@link Route.prerender}.
   *
   * @default false
   */
  authRequired?: boolean;
}

/**
 * A URL path mapped to a {@link Page}.
 *
 * Create one with the {@link route} constructor.
 *
 * See [Routing](https://wasp.sh/docs/advanced/routing) for path patterns
 * (dynamic segments, optional segments, splats).
 *
 * @category Parts
 */
export interface Route extends BasePart<"route"> {
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
   * If `true`, Wasp renders this page to static HTML at build time. Useful
   * for SEO and AI crawlers.
   *
   * Only works on static paths and cannot be combined with
   * {@link Page.authRequired}. See
   * [Prerendering](https://wasp.sh/docs/advanced/prerendering).
   *
   * @default false
   */
  prerender?: boolean;
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
 * @category Parts
 */
export interface Query extends BasePart<"query"> {
  /**
   * Reference to the Query's NodeJS implementation. The implementation can be
   * async and receives two positional arguments: the caller-provided `args`
   * payload and a Wasp-provided `context` containing declared entities and,
   * when auth is enabled for the Query, the current user.
   */
  fn: Reference<AnyFunction>;
  /**
   * Entities the query reads from. Wasp injects a Prisma delegate for each
   * one into the query's `context.entities`, and uses the list to invalidate
   * the client-side cache when related actions run.
   */
  entities?: EntityName[];
  /**
   * If `true`, the query receives `context.user`. Defaults to `true` when
   * the app has auth enabled, and `false` otherwise.
   *
   * You can only set this field when app-level auth is enabled.
   */
  auth?: boolean;
}

/**
 * A server-side write operation, callable from the client and the server.
 *
 * Create one with the {@link action} constructor.
 *
 * See [Actions](https://wasp.sh/docs/data-model/operations/actions).
 *
 * @category Parts
 */
export interface Action extends BasePart<"action"> {
  /**
   * Reference to the Action's NodeJS implementation. The implementation can be
   * async and receives two positional arguments: the caller-provided `args`
   * payload and a Wasp-provided `context` containing declared entities and,
   * when auth is enabled for the Action, the current user.
   */
  fn: Reference<AnyFunction>;
  /**
   * Entities the action operates on. Wasp injects a Prisma delegate for each
   * one into the action's `context.entities`, and uses the list to
   * invalidate the related query caches on the client.
   */
  entities?: EntityName[];
  /**
   * If `true`, the action receives `context.user`. Defaults to `true` when
   * the app has auth enabled, and `false` otherwise.
   *
   * You can only set this field when app-level auth is enabled.
   */
  auth?: boolean;
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
 * @category Parts
 */
export interface Api extends BasePart<"api"> {
  /** HTTP method this endpoint responds to. */
  method: HttpMethod;
  /** Express path of the endpoint (e.g. `"/webhooks/stripe"`). */
  path: string;
  /** Reference to the API's NodeJS implementation. */
  fn: Reference<AnyFunction>;
  /**
   * Reference to an Express middleware config function for this endpoint only.
   */
  middlewareConfigFn?: Reference<AnyFunction>;
  /**
   * Entities the handler operates on. Wasp injects a Prisma delegate for
   * each one into the handler's `context.entities`.
   */
  entities?: EntityName[];
  /**
   * If `true`, the handler requires the request to come from an
   * authenticated user and receives `context.user`. Defaults to `true` when
   * the app has auth enabled, and `false` otherwise. Set to `false` to skip
   * parsing the JWT from the Authorization header.
   */
  auth?: boolean;
}

/**
 * Shared middleware applied to every {@link Api} mounted under a path
 * prefix.
 *
 * Create one with the {@link apiNamespace} constructor.
 *
 * @category Parts
 */
export interface ApiNamespace extends BasePart<"apiNamespace"> {
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
 * @category Parts
 */
export interface Job extends BasePart<"job"> {
  /**
   * Reference to the async function that performs the job's work. It receives
   * the submitted args and a context containing the declared entities.
   */
  fn: Reference<AnyFunction>;
  /** Executor backing this job. */
  executor: JobExecutor;
  /** Cron schedule that runs the job automatically. */
  schedule?: Schedule;
  /**
   * Entities the worker operates on. Wasp injects a Prisma delegate for
   * each one into the worker's `context.entities`.
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
 * Supported job executors. Currently only `"PgBoss"` is available, which
 * stores jobs in the app's PostgreSQL database via
 * [pg-boss](https://github.com/timgit/pg-boss).
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
 * @category Parts
 */
export interface Crud extends BasePart<"crud"> {
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
 * Wasp's defaults. Default `get`, `update`, and `delete` implementations use
 * the field marked with `@id` in the Prisma schema as the entity ID.
 */
export interface CrudOperations
  extends Partial<Record<CrudOperation, CrudOperationOptions>> {}

/**
 * Operations a {@link Crud} can generate.
 */
export type CrudOperation = "get" | "getAll" | "create" | "update" | "delete";

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
   * The override receives the caller-provided `args` payload and a
   * Wasp-provided `context` containing the current user and the entity being
   * operated on. Use an override to validate or filter input before saving it;
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
 * A reference to your app's code. Prefer importing the value with
 * `with { type: "ref" }` (a {@link RefImport}); use a {@link RefObject} when a
 * direct reference import is not practical.
 *
 * See [Reference imports](https://wasp.sh/docs/general/spec#reference-imports).
 */
export type Reference<T> = RefImport<T> | RefObject;

/**
 * The result of `import X from "@src/..." with { type: "ref" }`: the imported
 * value itself, as passed directly to spec constructors.
 */
export type RefImport<T> = T;

export type {
  /**
   * Default-import variant of {@link RefObject}, written as
   * `{ import: 'default', alias: 'X', from: '@src/...' }`.
   */
  DefaultRefObject,
  /**
   * Named-import variant of {@link RefObject}, written as
   * `{ import: 'X', from: '@src/...' }`. An optional `alias` renames the
   * imported value.
   */
  NamedRefObject,
  /**
   * A reference object, pointing to a value in the project's `src` directory.
   * Used wherever the spec accepts a reference to user code.
   */
  RefObject,
} from "../refObject.js";

interface BasePart<Kind extends string> {
  /**
   * The internal Wasp type of this part. Used by the compiler.
   * You should not set this field directly, instead use the dedicated constructors for each part type.
   */
  kind: Kind;
}

/**
 * Structural type for a runtime Zod schema.
 *
 * Env schemas imported with reference imports are lowered to {@link RefObject}s
 * before mapping, but the public API still accepts runtime Zod objects
 * in `server.envValidationSchema` and `client.envValidationSchema`.
 *
 * To avoid depending on the `zod` package, Wasp structurally recognizes Zod 4
 * schemas via their documented library-author marker. See
 * https://zod.dev/library-authors.
 */
export type ZodSchema = {
  readonly _zod: {
    readonly def: object;
  };
};
