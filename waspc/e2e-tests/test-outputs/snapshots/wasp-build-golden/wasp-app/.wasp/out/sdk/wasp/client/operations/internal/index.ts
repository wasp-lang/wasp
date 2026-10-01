import { type OperationRoute, makeOperationRoute } from '@wasp.sh/lib-sdk-core'
export { type OperationRoute, makeOperationRoute } from '@wasp.sh/lib-sdk-core'
import { api, handleApiError } from '../../../api/index.js'
import { HttpMethod } from '../../index.js'
import { serialize, deserialize } from '../../../core/serialization/index.js'

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
