import { QueryClient } from '@tanstack/react-query'
import {
  type CreateQueryClientFn,
  getWaspQueryClientDefaults,
} from '@wasp/lib-sdk-core/browser'
import { createQueryClient as createQueryClient_ext } from "virtual:wasp/user/queryClient"

export type RegisteredCreateQueryClient = CreateQueryClientFn

// PRIVATE API
export const queryClient: QueryClient = createQueryClient_ext(getWaspQueryClientDefaults())
