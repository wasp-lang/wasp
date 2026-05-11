import { QueryClient } from '@tanstack/react-query'
import { freezeAndGetConfig } from './queryClient.config'

import { setup as setup_ext } from 'wasp/src/clientSetup'

await setup_ext()

const defaultQueryClientConfig = {};

const queryClient = new QueryClient(
  freezeAndGetConfig() ?? defaultQueryClientConfig
);

// PRIVATE API (framework code)
export const queryClientInitialized: Promise<QueryClient> = Promise.resolve(queryClient);

// PUBLIC API
export { configureQueryClient } from './queryClient.config';
