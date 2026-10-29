import type { ErrorMessage } from "@wasp/lib-auth/browser";
import { getErrorMessage } from "../../../errors.js";
import { WaspHttpError } from "../../../WaspHttpError.js";

export function getAuthErrorMessage(error: unknown): ErrorMessage {
  if (error instanceof WaspHttpError) {
    return {
      title: error.message,
      description: getHttpErrorDescription(error),
    };
  }
  // We only expect HTTP errors here, so we log anything else,
  // like a network failure, for the developer.
  console.error(error);
  return { title: getErrorMessage(error) };
}

function getHttpErrorDescription(error: WaspHttpError): string | undefined {
  // Auth endpoints put the details in `HttpError`'s data, e.g. `{ message }`.
  const responseJson = error.data as
    | { data?: { message?: unknown } }
    | undefined;
  const description = responseJson?.data?.message;

  return typeof description === "string" ? description : undefined;
}
