import express from 'express'

import { FooBar } from '@wasp/apis/types'
import { MiddlewareConfigFn } from '@wasp/middleware'

export const fooBar : FooBar = (req, res, context) => {
  console.log(req.body)
  console.log(context)
  res.json({ msg: `Hello, ${context.user?.username || "stranger"}!` })
}

export const fooBarMiddlewareFn : MiddlewareConfigFn = (middleware) => {
  console.log(`Removing all default middleware: ${middleware}`)
  console.log('Adding custom and express.text middlewares.')
  return [
    { name: 'custom.route',
      fn: (_req, _res, next) => {
        console.log('custom route middleware')
        next()
      }
    },
    {
      name: 'express.text',
      fn: express.text({ type: '*/*' })
    }
  ]
}
