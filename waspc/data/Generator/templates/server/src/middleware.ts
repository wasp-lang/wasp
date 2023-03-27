import express from 'express'
import cookieParser from 'cookie-parser'
import logger from 'morgan'
import cors from 'cors'
import helmet from 'helmet'

import config from './config.js'

export type MiddlewareConfigFn = (middleware: MiddlewareConfig[]) => MiddlewareConfig[]

export type MiddlewareConfig = { name: string; fn: express.RequestHandler }

// NOTE: These are installed on a per-router or per-route basis.
// TODO: Have a global function to operate on the middleware array.
export const defaultMiddleware: MiddlewareConfig[] = [
  { name: 'helmet', fn: helmet() },
  // TODO: Consider allowing users to provide an ENV variable or function to further configure CORS setup.
  { name: 'cors', fn: cors({ origin: config.allowedCORSOrigins }) },
  { name: 'logger', fn: logger('dev') },
  { name: 'express.json', fn: express.json() },
  { name: 'express.urlencoded', fn: express.urlencoded({ extended: false }) },
  { name: 'cookieParser', fn: cookieParser() },
]

export const defaultMiddlewareArray: express.RequestHandler[] = toMiddlewareArray(defaultMiddleware)

export function toMiddlewareArray(middleware: MiddlewareConfig[]): express.RequestHandler[] {
  return middleware.map(({ fn }) => fn)
}
