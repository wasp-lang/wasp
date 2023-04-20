import { BarBaz, FooBar, WebhookCallback } from '@wasp/apis/types'
import express from 'express'
import { MiddlewareConfig, MiddlewareConfigFn } from '@wasp/middleware'

export const fooBar: FooBar = (_req, res, context) => {
  res.json({ msg: `Hello, ${context?.user?.email}!` })
}

export const barBaz: BarBaz = (_req, res, _context) => {
  res.json({ msg: `Hello, stranger!` })
}

export const fooBarNamespaceMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  // console.log('fooBarNamespaceMiddlewareFn: Ignoring all default middleware except cors.')

  const updatedMiddlewareConfig: MiddlewareConfig = new Map([
    ['express.text', express.text({ type: '*/*' })]
  ])

  if (middlewareConfig.has('cors')) {
    updatedMiddlewareConfig.set('cors', middlewareConfig.get('cors')!)
  }

  return updatedMiddlewareConfig
}

export const fooBarMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  // console.log('fooBarMiddlewareFn: Adding custom middleware for route.')

  const customMiddleware : express.RequestHandler = (_req, _res, next) => {
    console.log('fooBarMiddlewareFn: custom route middleware')
    next()
  }

  middlewareConfig.set('custom.route', customMiddleware)

  return middlewareConfig
}

export const webhookCallback: WebhookCallback = (req, res, _context) => {
  res.json({ msg: req.body.length })
}

export const webhookCallbackMiddlewareFn: MiddlewareConfigFn = (middlewareConfig) => {
  // console.log('webhookCallbackMiddlewareFn: Swap express.json for express.raw')
  
  middlewareConfig.delete('express.json')
  middlewareConfig.set('express.raw', express.raw({ type: '*/*' }))

  return middlewareConfig
}
