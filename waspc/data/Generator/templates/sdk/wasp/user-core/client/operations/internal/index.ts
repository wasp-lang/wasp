import { api, handleApiError } from 'wasp/client/api'
import { deserialize, serialize } from 'wasp/core/serialization'
import { HttpMethod } from '../../../../core/client/index'

// PRIVATE API
export type OperationRoute = { method: HttpMethod.Post, path: string }

// PRIVATE API
export async function callOperation(operationRoute: OperationRoute, args: any) {
  try {
    const serializedArgs = serialize(args)
    const response = await api.post(operationRoute.path, serializedArgs)
    return deserialize(response.data)
  } catch (error) {
    throw handleApiError(error)
  }
}

// PRIVATE API
export function makeOperationRoute(relativeOperationRoute: string): OperationRoute {
  return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` }
}
