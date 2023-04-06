import api, { handleApiError } from '../api'
import { HttpMethod } from '../types'
import { 
  serialize as superjsonSerialize,
  deserialize as superjsonDeserialize,
 } from 'superjson'

export type OperationRoute = { method: HttpMethod, path: string }

export async function callOperation(operationRoute: OperationRoute & { method: HttpMethod.Post }, args: any) {
  try {
    const superjsonArgs = superjsonSerialize(args)
    const response = await api.post(operationRoute.path, superjsonArgs)
    return superjsonDeserialize(response.data)
  } catch (error) {
    handleApiError(error)
  }
}

export function makeOperationRoute(relativeOperationRoute: string): OperationRoute {
  return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` }
}
