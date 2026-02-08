import { api, handleApiError } from 'wasp/client/api'
import { HttpMethod } from 'wasp/client'
import { serialize, deserialize } from 'wasp/core/serialization'

// PRIVATE API
export type OperationRoute = { method: HttpMethod.Post, path: string }

// Works correctly in SSR: waspConfig.ts replaces `typeof window` with
// `"undefined"` in the SSR bundle, so this evaluates to false on the server.
const isBrowser = typeof window !== 'undefined'

// PRIVATE API
export async function callOperation(operationRoute: OperationRoute, args: any) {
  // During SSR, operations cannot be executed because:
  // 1. The backend server may not be available
  // 2. There's no user session/auth context
  // 3. Side effects (actions) shouldn't run during server rendering
  if (!isBrowser) {
    throw new Error(
      `Wasp operations cannot be called during SSR. ` +
      `If you need to call an operation in a setup function or component, ` +
      `wrap it in a useEffect hook or check for 'typeof window !== "undefined"'.`
    )
  }

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
