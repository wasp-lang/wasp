import { QueryClient, QueryClientConfig } from '@tanstack/react-query'

const defaultQueryClientConfig = {};

let queryClientConfig: QueryClientConfig,
  resolveQueryClientInitialized: (...args: any[]) => any,
  isQueryClientInitialized: boolean,
  _queryClientInstance: QueryClient | null = null;

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
  _queryClientInstance = queryClient;
  isQueryClientInitialized = true;
  resolveQueryClientInitialized(queryClient);
}

// PRIVATE API (framework code)
// Synchronous access to the QueryClient instance. Returns the client if
// initializeQueryClient() has already been called, or null otherwise.
// This is used by WaspApp to provide an initial value for useState so the
// first client render matches the SSR output (avoiding hydration mismatch).
export function getQueryClientSync(): QueryClient | null {
  return _queryClientInstance;
}
