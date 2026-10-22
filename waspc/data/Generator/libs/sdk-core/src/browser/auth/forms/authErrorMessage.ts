import type { ErrorMessage } from "@wasp.sh/lib-auth/browser";
import { getErrorMessage } from "../../../errors.js";
import { HttpError } from "../../../HttpError.js";

export function getAuthErrorMessage(error: unknown): ErrorMessage {
  if (error instanceof HttpError) {
    // Auth endpoints put the details in `HttpError`'s data, e.g. `{ message }`.
    const description = error.data?.message;
    return {
      title: error.message,
      description: typeof description === "string" ? description : undefined,
    };
  }
  // We only expect HTTP errors here, so we log anything else,
  // like a network failure, for the developer.
  console.error(error);
  return { title: getErrorMessage(error) };
}
