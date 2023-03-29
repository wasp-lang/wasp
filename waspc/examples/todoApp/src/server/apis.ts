import express from 'express'

import { FooBar } from '@wasp/apis/types'
import { MiddlewareConfigFn } from '@wasp/middleware'

export const fooBar: FooBar = (req, res, context) => {
  console.log(req.body)
  console.log(context)
  res.json({ msg: `Hello, ${context.user?.username || "stranger"}!` })
}

export const fooBarMiddlewareFn: MiddlewareConfigFn = (middleware) => {
  console.log('Adding custom and express.text middlewares for route.')

  const customMiddleware : express.RequestHandler = (_req, _res, next) => {
    console.log('custom route middleware')
    next()
  }

  const updatedMiddleware = new Map([
    ['custom.route', customMiddleware],
    ['express.text', express.text({ type: '*/*' })]
  ])

  console.log('Ignoring all default middleware except cors.')

  if (middleware.has('cors')) {
    updatedMiddleware.set('cors', middleware.get('cors')!)
  }

  return updatedMiddleware
}
