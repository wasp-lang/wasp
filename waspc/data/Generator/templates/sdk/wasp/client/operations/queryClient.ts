{{={= =}=}}
import { QueryClient } from '@tanstack/react-query'
import { freezeAndGetConfig } from './queryClient.config'

{=# setupFn.isDefined =}
{=& setupFn.importStatement =}

await {= setupFn.importIdentifier =}()
{=/ setupFn.isDefined =}

const defaultQueryClientConfig = {};

const queryClient = new QueryClient(
  freezeAndGetConfig() ?? defaultQueryClientConfig
);

// PRIVATE API (framework code)
export const queryClientInitialized: Promise<QueryClient> = Promise.resolve(queryClient);
