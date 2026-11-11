import { QueryClient } from '@tanstack/react-query'
import {
  type CreateQueryClientFn,
  getWaspQueryClientDefaults,
} from '@wasp/lib-sdk-core/browser'

export type RegisteredCreateQueryClient = CreateQueryClientFn

// PRIVATE API
export const queryClient: QueryClient = new QueryClient(getWaspQueryClientDefaults())
