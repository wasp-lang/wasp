/**
 * Runtime configuration and environment variables of your app's client.
 *
 * @module
 */

/**
 * The client's configuration object.
 *
 * @example
 * ```ts
 * import { config } from "wasp/client";
 *
 * console.log(config.apiUrl);
 * ```
 *
 * @see [Accessing the configuration](https://wasp.sh/docs/advanced/accessing-app-config)
 */
export declare const config: ClientConfig;

/** The type of the {@link config} object. */
export type ClientConfig = {
  /**
   * The URL of your server (the app's backend), without a trailing slash, from
   * the {@linkcode ClientEnv.REACT_APP_API_URL | REACT_APP_API_URL} env var.
   */
  apiUrl: string;
};

/**
 * The client's validated environment variables.
 *
 * It contains the env vars that Wasp defines (like `REACT_APP_API_URL`) and the
 * ones you validate with the `client.envValidationSchema` field of the app
 * spec. It leaves out any other env var, even if its name starts with
 * `REACT_APP_`.
 *
 * Wasp merges your validation schema with its own and validates
 * `import.meta.env` with it, both when Vite starts (in development and when
 * building) and when the client code loads.
 *
 * Client env vars are embedded into the client code at build time, so anyone
 * can read them. Never store secrets in them. Their names must start with
 * `REACT_APP_`.
 *
 * You can also read `import.meta.env.REACT_APP_SOME_VAR_NAME` directly, but we
 * don't recommend it: `import.meta.env` isn't validated, and missing env vars
 * can cause runtime errors.
 *
 * @example
 * ```ts
 * import { env } from "wasp/client";
 *
 * // Wasp-defined
 * const apiUrl = env.REACT_APP_API_URL;
 *
 * // User-defined
 * const analyticsId = env.REACT_APP_ANALYTICS_ID;
 * ```
 *
 * @see [Client env vars](https://wasp.sh/docs/advanced/env-vars#client-env-vars)
 * @see [Custom env var validations](https://wasp.sh/docs/advanced/env-vars#custom-env-var-validations)
 * @see {@link "env"!defineEnvValidationSchema | defineEnvValidationSchema} to define your validation schema.
 */
export declare const env: ClientEnv;

/**
 * The type of {@link env}.
 *
 * @see [Wasp client env vars](https://wasp.sh/docs/advanced/env-vars#wasp-client-env-vars)
 */
type ClientEnv = {
  /**
   * The URL of your server, which the client uses to call it. Wasp sets it
   * during development (`wasp start`). Your deployed app requires it: set it to
   * the URL of your server as the user's browser sees it (with the DNS and
   * proxies considered).
   */
  REACT_APP_API_URL: string;
  /**
   * The env vars from your `client.envValidationSchema`, with the types your
   * Zod schema outputs.
   */
  [name: string]: unknown;
};

/**
 * The HTTP methods that a {@link Route} can use. These are the methods of
 * Wasp's Operations and APIs, except for the `ALL` method that you can declare
 * an API with.
 *
 * Use it to describe a {@link Route}, for example when mocking an API with the
 * `mockApi` helper that {@link "client/test"!mockServer | mockServer} returns.
 */
export { HttpMethod } from "@wasp/lib-sdk-core";

/**
 * An HTTP method and path that identify an endpoint of your server.
 *
 * @example
 * ```ts
 * import { HttpMethod, type Route } from "wasp/client";
 *
 * const route: Route = { method: HttpMethod.Get, path: "/foo/bar" };
 * ```
 */
export type { Route } from "@wasp/lib-sdk-core";
