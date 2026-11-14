import type { ErrorMessage } from "@wasp/lib-auth/browser";
import { getErrorMessage } from "../../../errors.js";
import { HttpError } from "../../../HttpError.js";
import { handleClientError } from "../../errors.js";

export function getAuthErrorMessage(error: unknown): ErrorMessage {
  if (error instanceof HttpError) {
    // Auth endpoints put the details in `HttpError`'s data, e.g. `{ message }`.
    const description =
      typeof error.data?.message === "string" ? error.data.message : undefined;
    return { title: error.message, description };
  }
  // We only expect HTTP errors here, so we report anything else,
  // like a network failure, for the developer.
  handleClientError(error, { source: "authForm" });
  return { title: getErrorMessage(error) };
}
