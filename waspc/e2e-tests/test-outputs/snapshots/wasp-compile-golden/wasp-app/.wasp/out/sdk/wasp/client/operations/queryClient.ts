import { QueryClient } from '@tanstack/react-query'
import { freezeAndGetConfig } from './queryClient.config'


const defaultQueryClientConfig = {};

const queryClient = new QueryClient(
  freezeAndGetConfig() ?? defaultQueryClientConfig
);

// PRIVATE API (framework code)
export const queryClientInitialized: Promise<QueryClient> = Promise.resolve(queryClient);
