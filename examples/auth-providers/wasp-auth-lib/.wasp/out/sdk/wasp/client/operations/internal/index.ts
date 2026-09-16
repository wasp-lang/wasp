import { api, handleApiError } from '../../../api/index.js'
import { HttpMethod } from '../../index.js'
import { serialize, deserialize } from '../../../core/serialization/index.js'

// PRIVATE API
export type OperationRoute = { method: HttpMethod.Post, path: string }

// PRIVATE API
export async function callOperation(operationRoute: OperationRoute, args: any) {
  try {
    const serializedArgs = serialize(args)
    const json = await api.post(operationRoute.path, {
      json: serializedArgs,
    }).json()
    return deserialize(json as any)
  } catch (error) {
    throw handleApiError(error)
  }
}

// PRIVATE API
export function makeOperationRoute(relativeOperationRoute: string): OperationRoute {
  return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` }
}
