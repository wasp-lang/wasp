import { type RequestHandler } from 'express'

export type MiddlewareConfig = Map<string, RequestHandler>

export type MiddlewareConfigFn = (middlewareConfig: MiddlewareConfig) => MiddlewareConfig

