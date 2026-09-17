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
export function makeOperationRoute(operationPath: string): OperationRoute {
  return { method: HttpMethod.Post, path: operationPath }
}

// PRIVATE API
// A query's cache key is its route's path without the leading slash.
// Users see these keys (e.g. in optimistic updates), so the format must not change.
export function makeQueryCacheKeyFromPath(routePath: string): string[] {
  return [routePath.replace(/^\//, '')]
}
