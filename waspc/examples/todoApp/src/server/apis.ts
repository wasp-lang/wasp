import express from 'express'

import { FooBar } from '@wasp/apis/types'
import { MiddlewareConfigFn } from '@wasp/middleware'

export const fooBar: FooBar = (req, res, context) => {
  console.log(req.body)
  console.log(context)
  res.json({ msg: `Hello, ${context.user?.username || "stranger"}!` })
}

export const fooBarMiddlewareFn: MiddlewareConfigFn = (middleware) => {
  console.log('Removing all default middleware except cors.')
  console.log('Adding custom and express.text middlewares.')

  return new Map([
    ['cors', middleware.get('cors')],
    ['custom.route',
      (_req, _res, next) => {
        console.log('custom route middleware')
        next()
      }
    ],
    ['express.text', express.text({ type: '*/*' })]
  ])
}
