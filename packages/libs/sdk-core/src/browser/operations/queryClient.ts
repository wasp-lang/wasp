import {
  isCancelledError,
  QueryClient,
  type QueryClientConfig,
} from "@tanstack/react-query";
import { HttpError } from "../../HttpError.js";
import { handleClientError, type ClientErrorContext } from "../errors.js";

// Same as react-query's default.
const MAX_QUERY_RETRIES = 3;

// 408 (Request Timeout) and 429 (Too Many Requests) are client errors that can
// succeed on a later attempt, so we keep retrying them.
const RETRYABLE_CLIENT_ERROR_STATUS_CODES = new Set([408, 429]);

let queryClientConfig: QueryClientConfig | undefined,
  resolveQueryClientInitialized: (...args: any[]) => any,
  isQueryClientInitialized: boolean;

export const queryClientInitialized: Promise<QueryClient> = new Promise(
  (resolve) => {
    resolveQueryClientInitialized = resolve;
  },
);

export function configureQueryClient(config: QueryClientConfig): void {
  if (isQueryClientInitialized) {
    throw new Error(
      "Attempted to configure the QueryClient after initialization",
    );
  }

  queryClientConfig = config;
}

export function initializeQueryClient(): void {
  const queryClient = new QueryClient(
    withWaspQueryDefaults(queryClientConfig ?? {}),
  );
  listenForQueryClientErrors(queryClient);
  isQueryClientInitialized = true;
  resolveQueryClientInitialized(queryClient);
}

// We subscribe to the caches instead of passing `onError` callbacks, so we
// don't replace the caches users pass through `configureQueryClient`.
function listenForQueryClientErrors(queryClient: QueryClient): void {
  queryClient.getQueryCache().subscribe((event) => {
    if (event.type === "updated" && event.action.type === "error") {
      reportUnlessCancelled(event.action.error, { source: "query" });
    }
  });
  queryClient.getMutationCache().subscribe((event) => {
    if (event.type === "updated" && event.action.type === "error") {
      reportUnlessCancelled(event.action.error, { source: "action" });
    }
  });
}

// TanStack Query cancels queries on purpose, e.g. before optimistic updates.
function reportUnlessCancelled(
  error: unknown,
  context: ClientErrorContext,
): void {
  if (!isCancelledError(error)) {
    handleClientError(error, context);
  }
}

function withWaspQueryDefaults(config: QueryClientConfig): QueryClientConfig {
  return {
    ...config,
    defaultOptions: {
      ...config.defaultOptions,
      queries: {
        retry: shouldRetryQuery,
        ...config.defaultOptions?.queries,
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
    error instanceof HttpError &&
    error.statusCode >= 400 &&
    error.statusCode < 500 &&
    !RETRYABLE_CLIENT_ERROR_STATUS_CODES.has(error.statusCode)
  );
}
