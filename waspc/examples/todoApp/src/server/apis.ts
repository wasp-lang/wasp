import { BarBaz, FooBar } from '@wasp/apis/types'
import express from 'express'
import { MiddlewareConfigFn } from '@wasp/middleware'

export const fooBar: FooBar = (_req, res, context) => {
  res.set('Access-Control-Allow-Origin', '*') // Example of modifying headers to override Wasp default CORS middleware.
  res.json({ msg: `Hello, ${context.user.email}!` })
}

export const barBaz: BarBaz = (_req, res, _context) => {
  res.set('Access-Control-Allow-Origin', '*')
  res.json({ msg: `Hello, stranger!` })
}

export const heyYoMiddlewareFn: MiddlewareConfigFn = (middleware) => {
  const updatedMiddleware = new Map([
    ['express.text', express.text({ type: '*/*' })]
  ])

  console.log('Ignoring all default middleware except cors.')

  if (middleware.has('cors')) {
    updatedMiddleware.set('cors', middleware.get('cors')!)
  }

  return updatedMiddleware
}

export const fooBarMiddlewareFn: MiddlewareConfigFn = (_middleware) => {
  console.log('Adding custom middleware for route.')

  const customMiddleware : express.RequestHandler = (_req, _res, next) => {
    console.log('custom route middleware')
    next()
  }

  const updatedMiddleware = new Map([
    ['custom.route', customMiddleware]
  ])

  return updatedMiddleware
}
