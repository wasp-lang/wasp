import type { QueryClient, QueryClientConfig } from "@tanstack/react-query";
import { WaspHttpError } from "../../WaspHttpError.js";

/**
 * A function that creates the app's `QueryClient`. You can pass it to the
 * `client.createQueryClient` field of the app spec.
 *
 * @param waspDefaults Wasp's default `QueryClient` config. Pass it to the
 * `QueryClient` constructor to keep Wasp's default behavior (e.g., not retrying
 * queries that fail with a client error).
 */
export type CreateQueryClientFn = (
  waspDefaults: QueryClientConfig,
) => QueryClient;

// Same as react-query's default.
const MAX_QUERY_RETRIES = 3;

// 408 (Request Timeout) and 429 (Too Many Requests) are client errors that can
// succeed on a later attempt, so we keep retrying them.
const RETRYABLE_CLIENT_ERROR_STATUS_CODES = new Set([408, 429]);

export function getWaspQueryClientDefaults(): QueryClientConfig {
  return {
    defaultOptions: {
      queries: {
        retry: shouldRetryQuery,
      },
    },
  };
}

// Client errors (4xx) won't go away by asking again, so retrying them only
// delays showing the error to the user.
function shouldRetryQuery(failureCount: number, error: unknown): boolean {
  if (failureCount >= MAX_QUERY_RETRIES) {
    return false;
  }

  return !(
    error instanceof WaspHttpError &&
    error.statusCode >= 400 &&
    error.statusCode < 500 &&
    !RETRYABLE_CLIENT_ERROR_STATUS_CODES.has(error.statusCode)
  );
}
