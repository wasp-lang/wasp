import express from 'express'
import cors from 'cors'
import type { MiddlewareConfigFn } from 'wasp/server/middleware'
import config from 'wasp/server/config'

export const serverMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  // Example of adding an extra domains to CORS.
  middlewareConfig.set('cors', cors({
    origin: [config.frontendUrl, 'https://example1.com', 'https://example2.com']
  }))
  return middlewareConfig
}

export const fooBarNamespace: MiddlewareConfigFn = (middlewareConfig) => {
  const customMiddleware: express.RequestHandler = (_req, _res, next) => {
    console.log('fooBarNamespaceMiddlewareFn: custom middleware')
    next()
  }

  middlewareConfig.set('custom.middleware', customMiddleware)

  return middlewareConfig
}
