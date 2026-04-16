import { QueryClient, QueryClientConfig } from '@tanstack/react-query'

const defaultQueryClientConfig = {};

let queryClientConfig: QueryClientConfig | undefined;
let isQueryClientInitialized = false;

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
export function initializeQueryClient(): QueryClient {
  isQueryClientInitialized = true;
  return new QueryClient(queryClientConfig ?? defaultQueryClientConfig);
}
