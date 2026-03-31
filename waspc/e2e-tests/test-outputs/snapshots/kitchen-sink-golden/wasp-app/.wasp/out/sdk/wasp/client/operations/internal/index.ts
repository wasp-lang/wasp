import { api } from 'wasp/client/api'
import { HttpMethod } from 'wasp/client'
import { serialize, deserialize } from 'wasp/core/serialization'

// PRIVATE API
export type OperationRoute = { method: HttpMethod.Post, path: string }

// PRIVATE API
export async function callOperation(operationRoute: OperationRoute, args: any) {
  const serializedArgs = serialize(args)
  const response = await api(operationRoute.path, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(serializedArgs),
  })
  return deserialize(await response.json())
}

// PRIVATE API
export function makeOperationRoute(relativeOperationRoute: string): OperationRoute {
  return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` }
}
