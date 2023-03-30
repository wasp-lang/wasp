{{={= =}=}}
import express from 'express'
import cookieParser from 'cookie-parser'
import logger from 'morgan'
import cors from 'cors'
import helmet from 'helmet'

import config from './config.js'

{=# middlewareConfigFnDefined =}
{=& middlewareImportStatement =}
{=/ middlewareConfigFnDefined =}
{=^ middlewareConfigFnDefined =}
const {=& middlewareImportAlias =} = (m: MiddlewareConfig) => m
{=/ middlewareConfigFnDefined =}

export type MiddlewareConfig = Map<string, express.RequestHandler>

export type MiddlewareConfigFn = (middleware: MiddlewareConfig) => MiddlewareConfig

const _defaultMiddleware: MiddlewareConfig = new Map([
  ['helmet', helmet()],
  ['cors', cors({ origin: config.allowedCORSOrigins })],
  ['logger', logger('dev')],
  ['express.json', express.json()],
  ['express.urlencoded', express.urlencoded({ extended: false })],
  ['cookieParser', cookieParser()]
])

// TODO: change middlewareImportAlias name
const defaultMiddleware = {=& middlewareImportAlias =}(_defaultMiddleware)

export function getDefaultMiddleware(): MiddlewareConfig {
  // Return a clone so they can't mess up the Map for any other routes.
  return new Map(defaultMiddleware)
}

export const defaultMiddlewareArray: express.RequestHandler[] = toMiddlewareArray(defaultMiddleware)

export function toMiddlewareArray(middleware: MiddlewareConfig): express.RequestHandler[] {
  return Array.from(middleware.values())
}
