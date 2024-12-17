import { api, handleApiError } from 'wasp/client/api'
import { HttpMethod } from 'wasp/client'
import {
  serialize as superjsonSerialize,
  deserialize as superjsonDeserialize,
 } from 'superjson'

// PRIVATE API
export type OperationRoute = { method: HttpMethod.Post, path: string }

// PRIVATE API
export async function callOperation(operationRoute: OperationRoute, args: any) {
  try {
    const superjsonArgs = superjsonSerialize(args)
    const response = await api.post(operationRoute.path, superjsonArgs)
    return superjsonDeserialize(response.data)
  } catch (error) {
    throw handleApiError(error)
  }
}

// PRIVATE API
export function makeOperationRoute(relativeOperationRoute: string): OperationRoute {
  return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` }
}
