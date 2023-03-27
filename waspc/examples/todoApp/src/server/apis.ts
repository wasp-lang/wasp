import express from 'express'

import { FooBar } from '@wasp/apis/types'
import { MiddlewareConfig } from '@wasp/middleware'

export const fooBar : FooBar = (req, res, context) => {
  console.log(req.body)
  console.log(context)
  res.json({ msg: `Hello, ${context.user?.username || "stranger"}!` })
}

fooBar.middlewareFn = (middleware: MiddlewareConfig[]): MiddlewareConfig[] => {
  console.log(`Removing all default middleware: ${middleware}`)
  console.log('Adding express.raw middleware.')
  return [
    { name: 'custom',
      fn: (_req, _res, next) => {
        console.log('custom middleware')
        next()
      }
    },
    {
      name: 'express.text',
      fn: express.text({ type: '*/*' })
    }
  ]
}
