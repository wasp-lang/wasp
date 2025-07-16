import express from 'express'
import { prisma } from 'wasp/server'
import { defineHandler } from 'wasp/server/utils'
import { MiddlewareConfigFn, globalMiddlewareConfigForExpress } from '../../middleware/index.js'
import auth from 'wasp/core/auth'
import { type AuthUserData, makeAuthUserIfPossible } from 'wasp/auth/user'

import { fooBarNamespaceMiddlewareFn as _waspfooBarNamespacenamespaceMiddlewareConfigFn } from '../../../../../../src/server/apiNamespaces.js'

import { fooBar as _waspfooBarfn } from '../../../../../../src/server/apis.js'
import { fooBarMiddlewareFn as _waspfooBarmiddlewareConfigFn } from '../../../../../../src/server/apis.js'
import { fooBaz as _waspfooBazfn } from '../../../../../../src/server/apis.js'

const idFn: MiddlewareConfigFn = x => x

const _waspfooBazmiddlewareConfigFn = idFn

const router = express.Router()

router.use('/bar', globalMiddlewareConfigForExpress(_waspfooBarNamespacenamespaceMiddlewareConfigFn))

const fooBarMiddleware = globalMiddlewareConfigForExpress(_waspfooBarmiddlewareConfigFn)
router.get(
  '/foo/bar',
  [auth, ...fooBarMiddleware],
  defineHandler(
    (
      req: Parameters<typeof _waspfooBarfn>[0] & { user: AuthUserData | null },
      res: Parameters<typeof _waspfooBarfn>[1],
    ) => {
      const context = {
        user: makeAuthUserIfPossible(req.user),
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
  defineHandler(
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
