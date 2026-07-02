import { QueryClient, QueryClientConfig } from '@tanstack/react-query'

const defaultQueryClientConfig = {};

let queryClientConfig: QueryClientConfig,
  queryClient: QueryClient | undefined;

// PUBLIC API
export function configureQueryClient(config: QueryClientConfig): void {
  if (queryClient) {
    throw new Error(
      "Attempted to configure the QueryClient after initialization"
    );
  }

  queryClientConfig = config;
}

// PRIVATE API (framework code)
// Idempotent: the first call creates the QueryClient, later calls return it.
export function initializeQueryClient(): QueryClient {
  queryClient ??= new QueryClient(queryClientConfig ?? defaultQueryClientConfig);
  return queryClient;
}

// PRIVATE API (framework code)
export function getQueryClient(): QueryClient {
  if (!queryClient) {
    throw new Error(
      "Attempted to access the QueryClient before initialization. " +
        "The QueryClient is initialized in `wasp/client/app/client-setup`, " +
        "after the user-defined client setup function completes."
    );
  }

  return queryClient;
}
