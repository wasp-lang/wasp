// PUBLIC API
export { HttpError } from "./HttpError.js";

// PUBLIC API
/**
 * Best effort to get a message out of an unknown error.
 */
export function getErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}
