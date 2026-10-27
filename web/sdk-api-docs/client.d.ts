/**
 * Runtime configuration and environment variables of your app's client.
 *
 * @module
 */

import type { ClientConfig } from "@wasp.sh/lib-sdk-core";

/**
 * The client's configuration object.
 *
 * Its fields are:
 *
 * - `apiUrl`: the URL of your server (the app's backend). Set it with the
 *   `REACT_APP_API_URL` env var. Wasp sets it automatically during development
 *   when you run `wasp start`. In production, it should contain the URL of your
 *   server as the user's browser sees it (i.e., with the DNS and proxies
 *   considered). Wasp strips any trailing slash from it.
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

/**
 * The client's validated environment variables.
 *
 * It contains both the env vars that Wasp defines (like `REACT_APP_API_URL`)
 * and the ones you validate with the `client.envValidationSchema` field of the
 * app spec. Wasp merges your validation schema with its own and validates
 * `import.meta.env` with it when the client starts, so the object's type
 * includes the outputs of your schema.
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
 * @see [`defineEnvValidationSchema`](https://wasp.sh/docs/api/wasp/env/functions/defineEnvValidationSchema) to define your validation schema.
 */
export declare const env: ClientEnv;

/**
 * The type of {@link env}.
 *
 * The fields of your own `client.envValidationSchema` are also included, but
 * aren't listed here since they depend on your app.
 *
 * @see [Wasp client env vars](https://wasp.sh/docs/advanced/env-vars#wasp-client-env-vars)
 */
type ClientEnv = {
  /**
   * The URL of your server, which the client uses to call it. Wasp sets it
   * during development, but your deployed app requires it.
   */
  REACT_APP_API_URL: string;
  /**
   * The env vars from your `client.envValidationSchema`, with the types your
   * Zod schema outputs.
   */
  [name: string]: unknown;
};

/**
 * The HTTP methods that Wasp APIs and Operations can use.
 *
 * Use it to describe a {@link Route}, for example when mocking an API with
 * `mockApi` from `wasp/client/test`.
 */
export { HttpMethod } from "@wasp.sh/lib-sdk-core";

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
export type { Route } from "@wasp.sh/lib-sdk-core";

/** The type of the {@link config} object. */
export type { ClientConfig } from "@wasp.sh/lib-sdk-core";
