/**
 * An HTTP client for calling your app's server, including your
 * [custom APIs](https://wasp.sh/docs/features/apis).
 *
 * @module
 */

/**
 * The URL or request to fetch: a path relative to your server's URL (e.g.
 * `"/foo/bar"`), a `URL`, or a `Request`.
 */
type KyInput = string | URL | Request;

/**
 * The response promise that [ky](https://github.com/sindresorhus/ky) returns.
 *
 * It's a `Promise<Response>` with `Body` methods added for convenience, so you
 * can call, for example, `api.get("/foo/bar").json()` directly.
 *
 * This is a simplified version of ky's `ResponsePromise` type; see
 * [ky's docs](https://github.com/sindresorhus/ky#api) for all its members.
 */
interface KyResponsePromise extends Promise<Response> {
  /** Gets the response body as JSON. */
  json<T = unknown>(): Promise<T>;
  /** Gets the response body as text. */
  text(): Promise<string>;
  /** Gets the response body as a `Blob`. */
  blob(): Promise<Blob>;
  /** Gets the response body as an `ArrayBuffer`. */
  arrayBuffer(): Promise<ArrayBuffer>;
  /** Gets the response body as `FormData`. */
  formData(): Promise<FormData>;
}

/**
 * A [ky](https://github.com/sindresorhus/ky) instance. Each method's `options`
 * are ky's request options.
 *
 * This is a simplified version of ky's `KyInstance` type; see
 * [ky's docs](https://github.com/sindresorhus/ky#api) for all its members and
 * options.
 */
interface KyInstance {
  /** Sends a `GET` request. */
  get(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `POST` request. */
  post(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `PUT` request. */
  put(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `PATCH` request. */
  patch(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `DELETE` request. */
  delete(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `HEAD` request. */
  head(url: KyInput, options?: object): KyResponsePromise;
}

/**
 * A [ky](https://github.com/sindresorhus/ky) instance configured for your
 * app's server.
 *
 * It automatically prepends your server's URL to the paths you pass, adds the
 * logged-in user's authentication header, and clears the user's session from
 * the browser if the server responds with `401`. Since ky is built on `fetch`,
 * you can also consume streaming responses through `Response.body`.
 *
 * Non-2xx responses make ky throw an `HTTPError`; pass it through
 * {@link handleApiError} to get an
 * {@linkcode "errors"!HttpError | HttpError}.
 *
 * Unless it's declared with the `"ALL"` method, a custom API gets the default
 * middleware, including CORS, only on its own HTTP method, not on `OPTIONS`.
 * Requests to such an API that need a CORS preflight (e.g. with the auth
 * header or a JSON body) fail unless you also apply the middleware to the path
 * with an `apiNamespace`.
 *
 * @example
 * ```ts
 * import { api } from "wasp/client/api";
 *
 * const data = await api.get("/foo/bar").json();
 * ```
 *
 * @see [Using the API from the client](https://wasp.sh/docs/features/apis#using-the-api-from-the-client)
 * @see [Making sure CORS works](https://wasp.sh/docs/features/apis#making-sure-cors-works)
 */
export declare const api: KyInstance;

/**
 * Transforms an error thrown by {@link api} into the
 * {@linkcode "errors"!HttpError | HttpError} that the server sent.
 *
 * If the response body isn't the one Wasp sends for an `HttpError` (e.g. a
 * proxy answered), the error has ky's error message and no `data`.
 *
 * @param error The error that `api` threw.
 * @returns An `HttpError` if `error` comes from an HTTP response, otherwise
 * `error` unchanged.
 *
 * @example
 * ```ts
 * import { api, handleApiError } from "wasp/client/api";
 *
 * try {
 *   await api.post("/tasks", { json: { description: "Buy milk" } });
 * } catch (error) {
 *   throw handleApiError(error);
 * }
 * ```
 */
export declare function handleApiError(error: unknown): unknown;
