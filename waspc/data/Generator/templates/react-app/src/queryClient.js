import { QueryClient } from 'react-query'

let queryClientConfig, resolveQueryClientInitialized, isQueryClientInitialized

export const queryClientInitialized = new Promise(resolve => {
  resolveQueryClientInitialized = resolve
});

export function configureQueryClient(config) {
  if (isQueryClientInitialized) {
    throw new Error("Attempted to configure the QueryClient after initialization")
  }

  queryClientConfig = config
}

export function initializeQueryClient() {
  const queryClient = new QueryClient(queryClientConfig)
  isQueryClientInitialized = true;
  resolveQueryClientInitialized(queryClient)
}
