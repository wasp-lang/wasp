/**
 * Server-side APIs you can use in your Operations, APIs, Jobs, and server
 * setup code.
 *
 * You can only import `wasp/server` in server code.
 *
 * @module
 */

/**
 * Throw an `HttpError` from an Operation or API to send an HTTP error response
 * to the client.
 *
 * For security reasons, Wasp sends any other exception thrown in your server
 * code to the client as a `500` response, with all details removed. Throw an
 * `HttpError` when you want to pass error information to the client.
 *
 * The constructor takes:
 *
 * - `statusCode`: The HTTP status code. It must be an integer in the range
 *   `[400, 600)`.
 * - `message` (optional): The error message.
 * - `data` (optional): An object with any extra information about the error.
 * - `options` (optional): The standard `Error` options (e.g. `cause`), and
 *   `headers` to set on the response.
 *
 * Wasp sends the status code, `message`, and `data` to the client, which
 * rethrows the error with these fields. This happens for every status code,
 * so only put information in them that is safe to show to the user.
 *
 * @example
 * ```ts
 * import { HttpError } from "wasp/server";
 *
 * export const getTask = async ({ id }, context) => {
 *   if (!context.user) {
 *     throw new HttpError(401, "You must be logged in.");
 *   }
 *   // ...
 * };
 * ```
 *
 * @example
 * ```ts
 * import { HttpError } from "wasp/server";
 *
 * throw new HttpError(
 *   403, // status code
 *   "You can't do this!", // message
 *   { foo: "bar" }, // data
 * );
 * ```
 *
 * @see [Error handling](https://wasp.sh/docs/features/data/operations/queries#error-handling)
 */
export { HttpError } from "@wasp.sh/lib-sdk-core";

/**
 * The server's configuration, which Wasp computes from the
 * {@link env | environment variables}.
 *
 * @example
 * ```ts
 * import cors from "cors";
 * import { config, type MiddlewareConfigFn } from "wasp/server";
 *
 * export const serverMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
 *   // Example of adding extra domains to CORS.
 *   middlewareConfig.set(
 *     "cors",
 *     cors({
 *       origin: [...config.allowedCORSOrigins, "https://example1.com"],
 *     }),
 *   );
 *   return middlewareConfig;
 * };
 * ```
 */
export declare const config: ServerConfig;

/**
 * The type of {@link config}.
 */
type ServerConfig = {
  /** The environment the server runs in, from the `NODE_ENV` env var. */
  env: "development" | "production";
  /** Whether the server runs in development (`wasp start`). */
  isDevelopment: boolean;
  /** The port the server listens on, from the `PORT` env var. */
  port: number;
  /** The URL of the database, from the `DATABASE_URL` env var. */
  databaseUrl: string;
  /**
   * The URL of your app's client, without a trailing slash, from the
   * `WASP_WEB_CLIENT_URL` env var.
   */
  frontendUrl: string;
  /**
   * The URL of your app's server, without a trailing slash, from the
   * `WASP_SERVER_URL` env var.
   */
  serverUrl: string;
  /**
   * The origins the default CORS middleware allows.
   *
   * In development, it allows every origin. In production, it only allows
   * the origin of {@link ServerConfig.frontendUrl | `frontendUrl`}.
   *
   * @see [Middleware](https://wasp.sh/docs/advanced/server-customization/middleware)
   */
  allowedCORSOrigins: (string | RegExp)[];
  /**
   * Auth settings. Only present if your app uses
   * [auth](https://wasp.sh/docs/features/auth/overview).
   */
  auth?: {
    /** The secret for signing tokens, from the `JWT_SECRET` env var. */
    jwtSecret: string;
  };
};

/**
 * The validated server environment variables.
 *
 * Wasp validates `process.env` on server start against its own validation
 * schema, merged with the one you define in the `server.envValidationSchema`
 * field of the app spec. If validation fails, the server doesn't start.
 *
 * `env` contains both **Wasp-defined** env vars (described in
 * {@link ServerEnv}) and **user-defined** env vars (the fields of your
 * validation schema, with the types your schema outputs).
 *
 * You can use `process.env.SOME_SECRET` directly in your code, but we don't
 * recommend it, since `process.env` isn't validated and missing env vars can
 * cause runtime errors.
 *
 * @example
 * ```ts
 * import { env } from "wasp/server";
 *
 * // Wasp-defined
 * const serverUrl = env.WASP_SERVER_URL;
 *
 * // User-defined
 * const stripeApiKey = env.STRIPE_API_KEY;
 * ```
 *
 * @see [Env variables](https://wasp.sh/docs/advanced/env-vars)
 * @see [Custom env var validations](https://wasp.sh/docs/advanced/env-vars#custom-env-var-validations)
 */
export declare const env: ServerEnv;

/**
 * The Wasp-defined env vars in {@link env}.
 *
 * Wasp only includes the env vars of the features your app uses (e.g. the
 * `SMTP_*` ones only if you use the SMTP email sender). The fields of your
 * own `server.envValidationSchema` are also included, but aren't listed here
 * since they depend on your app.
 *
 * @see [Wasp server env vars](https://wasp.sh/docs/advanced/env-vars#wasp-server-env-vars)
 */
type ServerEnv = {
  /**
   * The environment the server runs in.
   *
   * @default "development"
   */
  NODE_ENV: "development" | "production";
  /**
   * The URL of the PostgreSQL database you want your app to use.
   */
  DATABASE_URL: string;
  /**
   * The URL of your app's client. The server uses it in various features,
   * e.g. linking to your app in emails.
   *
   * Wasp sets this in development, but your deployed app requires it.
   */
  WASP_WEB_CLIENT_URL: string;
  /**
   * The URL of your app's server. The server uses it in various features,
   * e.g. to redirect users when logging in with OAuth providers like Google
   * or GitHub.
   *
   * Wasp sets this in development, but your deployed app requires it.
   */
  WASP_SERVER_URL: string;
  /**
   * A random string of at least 32 characters, used to generate secure
   * tokens. Only present if your app uses auth.
   *
   * Defaults to `DEVJWTSECRET` in development.
   */
  JWT_SECRET?: string;
  /**
   * The port the server listens on for requests.
   *
   * Wasp sets this in development, but your deployed app requires it.
   */
  PORT: number;
  /**
   * Custom config for PgBoss, as a
   * [JSON env var](https://wasp.sh/docs/advanced/env-vars#json-env-vars).
   *
   * @see [Customizing PgBoss](https://wasp.sh/docs/features/jobs#pg_boss_new_options)
   */
  PG_BOSS_NEW_OPTIONS?: string | undefined;
  /**
   * If `true`, Wasp automatically sets user emails as verified in
   * development. Set it to the string `"true"` or `"false"`.
   *
   * @default false
   */
  SKIP_EMAIL_VERIFICATION_IN_DEV: boolean;
  /** The SMTP server host. Only present if you use the SMTP email sender. */
  SMTP_HOST?: string;
  /** The SMTP server port. Only present if you use the SMTP email sender. */
  SMTP_PORT?: number;
  /** The SMTP server username. Only present if you use the SMTP email sender. */
  SMTP_USERNAME?: string;
  /** The SMTP server password. Only present if you use the SMTP email sender. */
  SMTP_PASSWORD?: string;
  /** The SendGrid API key. Only present if you use the SendGrid email sender. */
  SENDGRID_API_KEY?: string;
  /** The Mailgun API key. Only present if you use the Mailgun email sender. */
  MAILGUN_API_KEY?: string;
  /** The Mailgun domain. Only present if you use the Mailgun email sender. */
  MAILGUN_DOMAIN?: string;
  /**
   * The Mailgun API URL. Only present if you use the Mailgun email sender.
   *
   * Useful if you want to use the EU API endpoint
   * (`https://api.eu.mailgun.net`).
   */
  MAILGUN_API_URL?: string | undefined;
  /** The Resend API key. Only present if you use the Resend email sender. */
  RESEND_API_KEY?: string;
  /** The Google OAuth client ID. Only present if you use Google auth. */
  GOOGLE_CLIENT_ID?: string;
  /** The Google OAuth client secret. Only present if you use Google auth. */
  GOOGLE_CLIENT_SECRET?: string;
  /** The GitHub OAuth client ID. Only present if you use GitHub auth. */
  GITHUB_CLIENT_ID?: string;
  /** The GitHub OAuth client secret. Only present if you use GitHub auth. */
  GITHUB_CLIENT_SECRET?: string;
  /** The Slack OAuth client ID. Only present if you use Slack auth. */
  SLACK_CLIENT_ID?: string;
  /** The Slack OAuth client secret. Only present if you use Slack auth. */
  SLACK_CLIENT_SECRET?: string;
  /** The Discord OAuth client ID. Only present if you use Discord auth. */
  DISCORD_CLIENT_ID?: string;
  /** The Discord OAuth client secret. Only present if you use Discord auth. */
  DISCORD_CLIENT_SECRET?: string;
  /** The Keycloak OAuth client ID. Only present if you use Keycloak auth. */
  KEYCLOAK_CLIENT_ID?: string;
  /** The Keycloak OAuth client secret. Only present if you use Keycloak auth. */
  KEYCLOAK_CLIENT_SECRET?: string;
  /** The URL of the Keycloak realm. Only present if you use Keycloak auth. */
  KEYCLOAK_REALM_URL?: string;
  /** The Microsoft tenant ID. Only present if you use Microsoft auth. */
  MICROSOFT_TENANT_ID?: string;
  /** The Microsoft OAuth client ID. Only present if you use Microsoft auth. */
  MICROSOFT_CLIENT_ID?: string;
  /** The Microsoft OAuth client secret. Only present if you use Microsoft auth. */
  MICROSOFT_CLIENT_SECRET?: string;
};

/**
 * The [Prisma Client](https://www.prisma.io/docs/orm/prisma-client) instance
 * Wasp uses to access the database.
 *
 * This is the same instance Wasp uses internally (e.g. for the `entities` in
 * your Operations' `context`). We recommend sticking with the Wasp-provided
 * mechanisms, and only using the Prisma Client directly if you need a feature
 * Wasp doesn't provide.
 *
 * You can only use it in server code.
 *
 * @example
 * ```ts
 * import { prisma } from "wasp/server";
 *
 * await prisma.task.create({
 *   data: { description: "Read the Entities doc", isDone: true },
 * });
 * ```
 *
 * @see [Using Entities directly](https://wasp.sh/docs/features/data/entities#using-entities-directly)
 * @see [Customising the Prisma Client](https://wasp.sh/docs/features/data/databases#customising-the-prisma-client)
 */
export declare const prisma: PrismaClient;

/**
 * The type of {@link prisma}.
 *
 * It's the type of the Prisma Client that Prisma generates from your Prisma
 * schema (or, if you customize it with `db.prismaSetupFn`, the type of the
 * client your function returns). It has one property per model, named after
 * the model in camelCase (e.g. `prisma.task` for the `Task` model), plus
 * Prisma's `$`-prefixed methods like `$transaction` and `$queryRaw`.
 *
 * Since it depends on your app's schema, this page only shows the minimal
 * structure every Prisma Client has. See the
 * [Prisma Client API reference](https://www.prisma.io/docs/orm/reference/prisma-client-reference)
 * for the rest.
 *
 * If your schema has no models, Prisma doesn't generate a client, and this
 * type (and {@link prisma}) is `null`.
 *
 * @see [Customising the Prisma Client](https://wasp.sh/docs/features/data/databases#customising-the-prisma-client)
 */
export type PrismaClient = {
  /** Connects to the database. Wasp calls this for you. */
  $connect(): Promise<void>;
  /** Disconnects from the database. */
  $disconnect(): Promise<void>;
};

/**
 * The type of the function you pass to the `server.setupFn` field of the app
 * spec. Wasp runs it on server start, before the server starts listening for
 * requests.
 *
 * It receives an object with:
 *
 * - `app`: The Express `Application`. Use it to add custom routes or
 *   middleware.
 * - `server`: The Node.js `http.Server`.
 *
 * You can use it to store values for later use, e.g. to set up a resource
 * your Operations use. We recommend putting the variable in the module where
 * you define the setup function, and exposing functions that read it. That
 * turns your module into a singleton that's constructed on server start.
 *
 * @example
 * Adding a custom route:
 *
 * ```ts
 * import type { Application } from "express";
 * import type { ServerSetupFn } from "wasp/server";
 *
 * export const mySetupFunction: ServerSetupFn = async ({ app }) => {
 *   addCustomRoute(app);
 * };
 *
 * function addCustomRoute(app: Application) {
 *   app.get("/customRoute", (_req, res) => {
 *     res.send("I am a custom route");
 *   });
 * }
 * ```
 *
 * @example
 * Storing values for later use:
 *
 * ```ts
 * import type { ServerSetupFn } from "wasp/server";
 *
 * let someResource = undefined;
 *
 * export const mySetupFunction: ServerSetupFn = async () => {
 *   someResource = await setUpSomeResource();
 * };
 *
 * export const getSomeResource = () => someResource;
 * ```
 *
 * @see [Server config](https://wasp.sh/docs/advanced/server-customization/server-config)
 */
export type { ServerSetupFn } from "@wasp.sh/lib-sdk-core/node";

/**
 * The type of a function that customizes Express middleware. You can pass it
 * to:
 *
 * - The `server.middlewareConfigFn` field of the app spec, to customize the
 *   global middleware of all Operations and APIs.
 * - The `middlewareConfigFn` option of an `api`, to customize the middleware
 *   of a single API.
 * - The `middlewareConfigFn` option of an `apiNamespace`, to customize the
 *   middleware of all APIs under a path.
 *
 * It receives the current middleware config, a `Map` from middleware names to
 * Express `RequestHandler`s, and returns the new one. You can add, replace, or
 * delete entries.
 *
 * Wasp's default global middleware are, by name: `"helmet"`, `"cors"`,
 * `"logger"`, `"express.json"`, `"express.urlencoded"`, and `"cookieParser"`.
 *
 * Please treat modifications to global middleware with extreme care, as they
 * affect all Operations and APIs.
 *
 * @example
 * ```ts
 * import cors from "cors";
 * import { config, type MiddlewareConfigFn } from "wasp/server";
 *
 * export const serverMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
 *   // Example of adding extra domains to CORS.
 *   middlewareConfig.set(
 *     "cors",
 *     cors({
 *       origin: [
 *         ...config.allowedCORSOrigins,
 *         "https://example1.com",
 *         "https://example2.com",
 *       ],
 *     }),
 *   );
 *   return middlewareConfig;
 * };
 * ```
 *
 * @see [Middleware](https://wasp.sh/docs/advanced/server-customization/middleware)
 */
export type { MiddlewareConfigFn } from "@wasp.sh/lib-sdk-core/node";

/**
 * The type of a seed function you list in the `db.seeds` field of the app
 * spec.
 *
 * A seed function is an async function that receives the {@link prisma}
 * client, and uses it to populate the database with some initial data. Since
 * it's server code, it can import other server code, e.g. your Actions.
 *
 * Run seed functions with `wasp db seed` (or `wasp db seed <seed-name>`).
 *
 * @param prisma The {@link prisma} client.
 *
 * @example
 * ```ts
 * import type { DbSeedFn } from "wasp/server";
 *
 * export const devSeedSimple: DbSeedFn = async (prisma) => {
 *   await prisma.task.create({
 *     data: { description: "Chase the cat", isDone: false },
 *   });
 * };
 * ```
 *
 * @see [Seeding the database](https://wasp.sh/docs/features/data/databases#seeding-the-database)
 */
export type DbSeedFn = (prisma: PrismaClient) => Promise<void>;

export {};
