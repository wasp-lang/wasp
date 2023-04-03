import api, { handleApiError } from '../api'
import { HttpMethod } from '../types'

export type OperationRoute = { method: HttpMethod, path: string }

export async function callOperation(operationRoute: OperationRoute & { method: HttpMethod.Post }, args: any) {
  try {
    const response = await api.post(operationRoute.path, args)
    return response.data
  } catch (error) {
    handleApiError(error)
  }
}

export function makeOperationRoute(relativeOperationRoute: string): OperationRoute {
  return { method: HttpMethod.Post, path: `/${relativeOperationRoute}` }
}
