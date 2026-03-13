import { config } from 'wasp/client'
import type { Route } from 'wasp/client'
import { serialize } from 'wasp/core/serialization'

// PUBLIC API
export type { Route } from 'wasp/client'

// PUBLIC API
export function getOperationRoute(operation: { route: Route }): {
  method: string
  url: string
} {
  return {
    method: operation.route.method,
    url: config.apiUrl + operation.route.path,
  }
}

// PUBLIC API
export function getApiUrl(route: Route): string {
  return config.apiUrl + route.path
}

// PUBLIC API
export function serializeForResponse(data: unknown): unknown {
  return serialize(data)
}
