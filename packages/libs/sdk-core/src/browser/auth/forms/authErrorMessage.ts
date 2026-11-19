import type { ErrorMessage } from "@wasp/lib-auth/browser";
import { getErrorMessage } from "../../../errors.js";
import { HttpError } from "../../../HttpError.js";
import { reportClientError } from "../../errors.js";

export function getAuthErrorMessage(error: unknown): ErrorMessage {
  // We only expect HTTP error responses the server sent on purpose here, so
  // anything else, like a network failure, is reported for the developer.
  reportClientError(error, { source: "authForm" });
  if (error instanceof HttpError) {
    // Auth endpoints put the details in the error's data, e.g. `{ message }`.
    const description =
      typeof error.data?.message === "string" ? error.data.message : undefined;
    return { title: error.message, description };
  }
  return { title: getErrorMessage(error) };
}
