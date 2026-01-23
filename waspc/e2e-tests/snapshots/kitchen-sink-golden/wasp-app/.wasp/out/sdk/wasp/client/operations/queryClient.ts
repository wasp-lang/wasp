import { QueryClient, QueryClientConfig } from '@tanstack/react-query'

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
  isQueryClientInitialized = true;
  resolveQueryClientInitialized(queryClient);
}
