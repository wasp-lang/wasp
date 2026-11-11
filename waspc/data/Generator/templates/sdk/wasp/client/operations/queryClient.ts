{{={= =}=}}
import { QueryClient } from '@tanstack/react-query'
import {
  type CreateQueryClientFn,
  getWaspQueryClientDefaults,
} from '@wasp/lib-sdk-core/browser'
{=# createQueryClient.isDefined =}
{=& createQueryClient.importStatement =}
{=/ createQueryClient.isDefined =}

export type RegisteredCreateQueryClient = CreateQueryClientFn

{=# createQueryClient.isDefined =}
// PRIVATE API
export const queryClient: QueryClient = {= createQueryClient.importIdentifier =}(getWaspQueryClientDefaults())
{=/ createQueryClient.isDefined =}
{=^ createQueryClient.isDefined =}
// PRIVATE API
export const queryClient: QueryClient = new QueryClient(getWaspQueryClientDefaults())
{=/ createQueryClient.isDefined =}
