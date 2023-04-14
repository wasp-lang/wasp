import express from 'express'
import cookieParser from 'cookie-parser'
import logger from 'morgan'
import cors from 'cors'
import helmet from 'helmet'

import config from '../config.js'

const globalMiddlewareConfigFn = (m: MiddlewareConfig) => m

export type MiddlewareConfig = Map<string, express.RequestHandler>

export type MiddlewareConfigFn = (middleware: MiddlewareConfig) => MiddlewareConfig

// This is the set of middleware Wasp supplies by default.
const defaultGlobalMiddleware: MiddlewareConfig = new Map([
  ['helmet', helmet()],
  ['cors', cors({ origin: config.allowedCORSOrigins })],
  ['logger', logger('dev')],
  ['express.json', express.json()],
  ['express.urlencoded', express.urlencoded({ extended: false })],
  ['cookieParser', cookieParser()]
])

// This is the global middleware that is the result of applying the user's modifications.
// It will be used as the basis for Operations and APIs (unless the latter is further customized).
const globalMiddleware = globalMiddlewareConfigFn(defaultGlobalMiddleware)

export function globalMiddlewareForExpress(middlewareConfigFn?: MiddlewareConfigFn): express.RequestHandler[] {
  if (!middlewareConfigFn) {
    return Array.from(globalMiddleware.values())
  }

  // Make a clone so they can't mess up the global Map for any other routes.
  const globalMiddlewareClone = new Map(globalMiddleware)
  const modifiedMiddleware = middlewareConfigFn(globalMiddlewareClone)
  return Array.from(modifiedMiddleware.values())
}
