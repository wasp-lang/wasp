import express from 'express'
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
import { MiddlewareConfigFn, globalMiddlewareConfigForExpress } from '../../middleware/index.js'
import auth from '../../core/auth.js'
import { type SanitizedUser } from '../../_types'


import { fooBar as _waspfooBarfn } from '../../ext-src/apis.js'
import { fooBaz as _waspfooBazfn } from '../../ext-src/apis.js'

const idFn: MiddlewareConfigFn = x => x

const _waspfooBarmiddlewareConfigFn = idFn
const _waspfooBazmiddlewareConfigFn = idFn

const router = express.Router()


const fooBarMiddleware = globalMiddlewareConfigForExpress(_waspfooBarmiddlewareConfigFn)
router.get(
  '/foo/bar',
  [auth, ...fooBarMiddleware],
  handleRejection(
    (
      req: Parameters<typeof _waspfooBarfn>[0] & { user: SanitizedUser },
      res: Parameters<typeof _waspfooBarfn>[1],
    ) => {
      const context = {
        user: req.user,
        entities: {
        },
      }
      return _waspfooBarfn(req, res, context)
    }
  )
)
const fooBazMiddleware = globalMiddlewareConfigForExpress(_waspfooBazmiddlewareConfigFn)
router.get(
  '/foo/baz',
  fooBazMiddleware,
  handleRejection(
    (
      req: Parameters<typeof _waspfooBazfn>[0],
      res: Parameters<typeof _waspfooBazfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return _waspfooBazfn(req, res, context)
    }
  )
)

export default router
