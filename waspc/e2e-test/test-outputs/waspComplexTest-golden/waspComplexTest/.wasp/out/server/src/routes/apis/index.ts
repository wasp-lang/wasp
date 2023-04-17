import express from 'express'
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
import { MiddlewareConfigFn, globalMiddlewareForExpress } from '../../middleware/index.js'
import auth from '../../core/auth.js'
import { type SanitizedUser } from '../../_types'


import { fooBar as fooBarfn } from '../../ext-src/apis.js'
import { fooBaz as fooBazfn } from '../../ext-src/apis.js'

const idFn: MiddlewareConfigFn = x => x

const fooBarmiddlewareConfigFn = idFn
const fooBazmiddlewareConfigFn = idFn

const router = express.Router()


const fooBarMiddleware = globalMiddlewareForExpress(fooBarmiddlewareConfigFn)
router.get(
  '/foo/bar',
  [auth, ...fooBarMiddleware],
  handleRejection(
    (
      req: Parameters<typeof fooBarfn>[0] & { user: SanitizedUser },
      res: Parameters<typeof fooBarfn>[1],
    ) => {
      const context = {
        user: req.user,
        entities: {
        },
      }
      return fooBarfn(req, res, context)
    }
  )
)
const fooBazMiddleware = globalMiddlewareForExpress(fooBazmiddlewareConfigFn)
router.get(
  '/foo/baz',
  fooBazMiddleware,
  handleRejection(
    (
      req: Parameters<typeof fooBazfn>[0],
      res: Parameters<typeof fooBazfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return fooBazfn(req, res, context)
    }
  )
)

export default router
