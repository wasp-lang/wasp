import { api, handleApiError } from '../../../api/index.js'
import { deserialize, serialize } from '../../../core/serialization/index.js'
import { HttpMethod } from '../../index.js'

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
