import { type RequestHandler } from 'express'

// PUBLIC API
export type MiddlewareConfigFn = (middlewareConfig: MiddlewareConfig) => MiddlewareConfig

// PRIVATE API
export type MiddlewareConfig = Map<string, RequestHandler>

