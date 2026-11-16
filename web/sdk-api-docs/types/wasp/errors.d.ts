/**
 * The base of Wasp's HTTP errors: what the server's
 * {@linkcode "server"!ServerHttpError | ServerHttpError} and the client's
 * {@linkcode "client"!ClientHttpError | ClientHttpError} have in common.
 *
 * You can import `wasp/errors` in both server and client code.
 *
 * @module
 */

export { HttpError, type HttpErrorData } from "@wasp/lib-sdk-core";
