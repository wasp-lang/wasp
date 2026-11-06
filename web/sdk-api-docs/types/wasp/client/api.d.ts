/**
 * An HTTP client for calling your app's server, including your custom APIs.
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
 * can call, for example, `api.get("/foo/bar").json()` directly. These methods
 * throw an `HTTPError` if the response status isn't in the `200...299` range.
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
 * A [ky](https://github.com/sindresorhus/ky) instance.
 *
 * This is a simplified version of ky's `KyInstance` type; see
 * [ky's docs](https://github.com/sindresorhus/ky#api) for all its members and
 * options.
 */
interface KyInstance {
  /** Sends a `GET` request. `options` are ky's request options. */
  get(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `POST` request. `options` are ky's request options. */
  post(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `PUT` request. `options` are ky's request options. */
  put(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `PATCH` request. `options` are ky's request options. */
  patch(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `DELETE` request. `options` are ky's request options. */
  delete(url: KyInput, options?: object): KyResponsePromise;
  /** Sends a `HEAD` request. `options` are ky's request options. */
  head(url: KyInput, options?: object): KyResponsePromise;
}

/**
 * A [ky](https://github.com/sindresorhus/ky) instance configured for your
 * app's server.
 *
 * It automatically prepends your server's URL to the paths you pass, adds the
 * logged-in user's authentication header, and logs the user out if the server
 * responds with `401`. Since ky is built on `fetch`, you can also consume
 * streaming responses through `Response.body`.
 *
 * Non-2xx responses make ky throw an `HTTPError`; pass it through
 * {@link handleApiError} to get a {@link WaspHttpError} with the server's
 * status code, message, and response body.
 *
 * Each custom API gets the default middleware, including CORS, only on its own
 * HTTP method, not on `OPTIONS`. Requests that need a CORS preflight (e.g., with
 * the auth header or a JSON body) fail unless you also apply the middleware to
 * the path with an `apiNamespace`.
 *
 * @example
 * ```tsx
 * import { useEffect } from "react";
 * import { api } from "wasp/client/api";
 *
 * async function fetchCustomRoute() {
 *   const data = await api.get("/foo/bar").json();
 *   console.log(data);
 * }
 *
 * export function Foo() {
 *   useEffect(() => {
 *     fetchCustomRoute();
 *   }, []);
 *   // ...
 * }
 * ```
 *
 * @see [Using the API from the client](https://wasp.sh/docs/features/apis#using-the-api-from-the-client)
 * @see [Making sure CORS works](https://wasp.sh/docs/features/apis#making-sure-cors-works)
 */
export declare const api: KyInstance;

/**
 * Transforms an error thrown by {@link api} into a {@link WaspHttpError}.
 *
 * If the error comes from an HTTP response, it returns a `WaspHttpError` with
 * the response's status code, the `message` from the response body (or ky's
 * error message if the body has none), and the parsed response body as `data`.
 * For an `HttpError` from `wasp/server`, the body is `{ message, data }`, so
 * that error's data is in `error.data.data`. It returns any other error
 * unchanged.
 *
 * @param error The error that `api` threw.
 * @returns A `WaspHttpError`, or `error` if it doesn't come from an HTTP
 * response.
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

/**
 * An error that a request to your server failed with.
 *
 * {@link handleApiError} creates it from the errors that {@link api} throws.
 * Its `data` is the parsed response body. For an `HttpError` from
 * `wasp/server`, the body is `{ message, data }`, so that error's data is in
 * `error.data.data`.
 */
export { WaspHttpError } from "@wasp/lib-sdk-core";
