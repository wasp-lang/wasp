import { type MiddlewareConfigFn } from 'wasp/server'
import { type BarBaz, type FooBar, type WebhookCallback } from 'wasp/server/api'
import express from 'express'

export const fooBar: FooBar = (_req, res, context) => {
  const username = context.user?.getFirstProviderUserId()

  res.json({ msg: `Hello, ${username}!` })
}

export const fooBarMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  // console.log('fooBarMiddlewareFn: Adding custom middleware for route.')

  const customMiddleware: express.RequestHandler = (_req, _res, next) => {
    console.log('fooBarMiddlewareFn: custom route middleware')
    next()
  }

  middlewareConfig.set('custom.route', customMiddleware)

  return middlewareConfig
}

export const barBaz: BarBaz = (_req, res, _context) => {
  res.json({ msg: `Hello, stranger!` })
}

export const barNamespaceMiddlewareFn: MiddlewareConfigFn = (
  middlewareConfig
) => {
  console.log('barNamespaceMiddlewareFn: Ignoring all default middleware.')

  middlewareConfig.set('custom.apiNamespace', (req, _res, next) => {
    console.log(
      `barNamespaceMiddlewareFn: custom middleware (path: ${req.path})`
    )
    next()
  })

  return middlewareConfig
}

export const webhookCallback: WebhookCallback = (req, res, _context) => {
  res.json({ msg: req.body.length })
}

export const webhookCallbackMiddlewareFn: MiddlewareConfigFn = (
  middlewareConfig
) => {
  // console.log('webhookCallbackMiddlewareFn: Swap express.json for express.raw')

  middlewareConfig.delete('express.json')
  middlewareConfig.set('express.raw', express.raw({ type: '*/*' }))

  return middlewareConfig
}
