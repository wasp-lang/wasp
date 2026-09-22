import { AsyncLocalStorage } from 'node:async_hooks'
import type { Request as ExpressRequest } from 'express'

/**
 * The request binding: the incoming request, bound to everything the
 * request's handling calls, however deep and however async. Wasp sets it once
 * at the Express boundary; the auth hooks read it as `req`, and `merge` checks
 * it. Outside a request (a job, a script) there is none.
 */
const requestContext = new AsyncLocalStorage<{ req: ExpressRequest }>()

// PRIVATE API
export function runWithRequest<T>(req: ExpressRequest, fn: () => T): T {
  return requestContext.run({ req }, fn)
}

// PRIVATE API
export function getCurrentRequest(): ExpressRequest | undefined {
  return requestContext.getStore()?.req
}
