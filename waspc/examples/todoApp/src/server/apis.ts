import { BarBaz, FooBar, WebhookCallback } from '@wasp/apis/types'
import express from 'express'
import { MiddlewareConfigFn } from '@wasp/middleware'

export const fooBar: FooBar = (_req, res, context) => {
  res.json({ msg: `Hello, ${context.user.email}!` })
}

export const barBaz: BarBaz = (_req, res, _context) => {
  res.json({ msg: `Hello, stranger!` })
}

export const fooBarNamespaceMiddlewareFn: MiddlewareConfigFn = (middleware) => {
  // console.log('fooBarNamespaceMiddlewareFn: Ignoring all default middleware except cors.')

  const updatedMiddleware = new Map([
    ['express.text', express.text({ type: '*/*' })]
  ])

  if (middleware.has('cors')) {
    updatedMiddleware.set('cors', middleware.get('cors')!)
  }

  return updatedMiddleware
}

export const fooBarMiddlewareFn: MiddlewareConfigFn = (middleware) => {
  // console.log('fooBarMiddlewareFn: Adding custom middleware for route.')

  const customMiddleware : express.RequestHandler = (_req, _res, next) => {
    console.log('fooBarMiddlewareFn: custom route middleware')
    next()
  }

  middleware.set('custom.route', customMiddleware)

  return middleware
}

export const webhookCallback: WebhookCallback = (req, res, _context) => {
  res.json({ msg: req.body.length })
}

export const webhookCallbackMiddlewareFn: MiddlewareConfigFn = (middleware) => {
  // console.log('webhookCallbackMiddlewareFn: Swap express.json for express.raw')
  
  middleware.delete('express.json')
  middleware.set('express.raw', express.raw({ type: '*/*' }))

  return middleware
}
