// PUBLIC API
export { HttpError, type HttpErrorData } from "@wasp/lib-sdk-core";

// PUBLIC API
// Undocumented: the wire format of an `HttpError`, for clients that don't use
// Wasp's own.
export {
  isHttpErrorBody,
  isHttpErrorStatusCode,
  toHttpErrorBody,
  type HttpErrorBody,
} from "@wasp/lib-sdk-core";
