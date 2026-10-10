// PUBLIC API
export { HttpError } from "./HttpError.js";

// PUBLIC API
/**
 * JavaScript can throw any value, so `catch` blocks get `unknown`.
 * Returns the message of an `Error`, or the thrown value as a string.
 */
export function getErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}
