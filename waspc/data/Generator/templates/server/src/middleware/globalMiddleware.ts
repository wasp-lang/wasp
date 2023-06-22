{{={= =}=}}
import express from 'express'
import cookieParser from 'cookie-parser'
import logger from 'morgan'
import cors from 'cors'
import helmet from 'helmet'

import config from '../config.js'

{=# globalMiddlewareConfigFn.isDefined =}
{=& globalMiddlewareConfigFn.importStatement =}
{=/ globalMiddlewareConfigFn.isDefined =}
{=^ globalMiddlewareConfigFn.isDefined =}
const {=& globalMiddlewareConfigFn.importAlias =} = (mc: MiddlewareConfig) => mc
{=/ globalMiddlewareConfigFn.isDefined =}

export type MiddlewareConfig = Map<string, express.RequestHandler>

export type MiddlewareConfigFn = (middlewareConfig: MiddlewareConfig) => MiddlewareConfig

// This is the set of middleware Wasp supplies by default.
// NOTE: Remember to update the docs of these change.
const defaultGlobalMiddlewareConfig: MiddlewareConfig = new Map([
  ['helmet', helmet()],
  ['cors', cors({ origin: config.allowedCORSOrigins })],
  ['logger', logger('dev')],
  ['express.json', express.json()],
  ['express.urlencoded', express.urlencoded({ extended: false })],
  ['cookieParser', cookieParser()]
])

// This is the global middleware that is the result of applying the user's modifications.
// It will be used as the basis for Operations and APIs (unless they are further customized).
const globalMiddlewareConfig: MiddlewareConfig = {=& globalMiddlewareConfigFn.importAlias =}(defaultGlobalMiddlewareConfig)

// This function returns an array of Express middleware to be used by a router. It optionally
// accepts a function that can modify the global middleware for specific route customization.
export function globalMiddlewareConfigForExpress(middlewareConfigFn?: MiddlewareConfigFn): express.RequestHandler[] {
  if (!middlewareConfigFn) {
    return Array.from(globalMiddlewareConfig.values())
  }

  // Make a clone so they can't mess up the global Map for any other routes calling this.
  const globalMiddlewareConfigClone = new Map(globalMiddlewareConfig)
  const modifiedMiddlewareConfig = middlewareConfigFn(globalMiddlewareConfigClone)
  return Array.from(modifiedMiddlewareConfig.values())
}
