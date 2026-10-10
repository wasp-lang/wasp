import type { ErrorMessage } from "@wasp.sh/lib-auth/browser";
import { reportClientError } from "../../../client/errors.js";
import { HttpError, getErrorMessage } from "../../../errors/index.js";

// PRIVATE API
export function getAuthErrorMessage(error: unknown): ErrorMessage {
  if (error instanceof HttpError) {
    return {
      title: error.message,
      description: getHttpErrorDescription(error),
    };
  }
  // We only expect HTTP errors here, so we report anything else,
  // like a network failure, for the developer.
  reportClientError(error, { source: "authForm" });
  return { title: getErrorMessage(error) };
}

function getHttpErrorDescription(error: HttpError): string | undefined {
  // Auth endpoints put the details in `HttpError`'s data, e.g. `{ message }`.
  const description = error.data?.message;
  return typeof description === "string" ? description : undefined;
}
