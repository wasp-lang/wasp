import { QueryClient, QueryClientConfig } from '@tanstack/react-query'

const defaultQueryClientConfig = {};

let queryClientConfig: QueryClientConfig,
  resolveQueryClientInitialized: (...args: any[]) => any,
  isQueryClientInitialized: boolean;

// Used in framework code, shouldn't be public
export const queryClientInitialized: Promise<QueryClient> = new Promise(
  (resolve) => {
    resolveQueryClientInitialized = resolve;
  }
);

// Used by users, should be public
export function configureQueryClient(config: QueryClientConfig): void {
  if (isQueryClientInitialized) {
    throw new Error(
      "Attempted to configure the QueryClient after initialization"
    );
  }

  queryClientConfig = config;
}

// Used in framework code, shouldn't be public
export function initializeQueryClient(): void {
  const queryClient = new QueryClient(
    queryClientConfig ?? defaultQueryClientConfig
  );
  isQueryClientInitialized = true;
  resolveQueryClientInitialized(queryClient);
}
