import {
  isCancelledError,
  QueryClient,
  type QueryClientConfig,
} from "@tanstack/react-query";
import { handleClientError } from "../errors.js";

const defaultQueryClientConfig = {};

let queryClientConfig: QueryClientConfig,
  resolveQueryClientInitialized: (...args: any[]) => any,
  isQueryClientInitialized: boolean;

// PRIVATE API (framework code)
export const queryClientInitialized: Promise<QueryClient> = new Promise(
  (resolve) => {
    resolveQueryClientInitialized = resolve;
  }
);

// PUBLIC API
export function configureQueryClient(config: QueryClientConfig): void {
  if (isQueryClientInitialized) {
    throw new Error(
      "Attempted to configure the QueryClient after initialization"
    );
  }

  queryClientConfig = config;
}

// PRIVATE API (framework code)
export function initializeQueryClient(): void {
  const queryClient = new QueryClient(
    queryClientConfig ?? defaultQueryClientConfig
  );
  reportQueryClientErrors(queryClient);
  isQueryClientInitialized = true;
  resolveQueryClientInitialized(queryClient);
}

// We subscribe to the caches instead of passing `onError` callbacks, so we
// don't replace the caches users pass through `configureQueryClient`.
function reportQueryClientErrors(queryClient: QueryClient): void {
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
  context: { source: "query" } | { source: "action" },
): void {
  if (!isCancelledError(error)) {
    handleClientError(error, context);
  }
}
