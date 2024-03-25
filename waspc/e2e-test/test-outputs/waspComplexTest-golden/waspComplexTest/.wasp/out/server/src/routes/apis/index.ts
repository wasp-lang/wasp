import express from 'express'
import { prisma } from 'wasp/server'
import { handleRejection } from 'wasp/server/utils'
import { MiddlewareConfigFn, globalMiddlewareConfigForExpress } from '../../middleware/index.js'
import auth from 'wasp/core/auth'
import { type AuthUser } from 'wasp/auth'

import { fooBarNamespaceMiddlewareFn as fooBarNamespaceMiddlewareFn__userDefined } from '../../../../../../src/server/apiNamespaces.js'

import { fooBar as fooBar__userDefined } from '../../../../../../src/server/apis.js'
import { fooBarMiddlewareFn as fooBarMiddlewareFn__userDefined } from '../../../../../../src/server/apis.js'
import { fooBaz as fooBaz__userDefined } from '../../../../../../src/server/apis.js'

const idFn: MiddlewareConfigFn = x => x

const router = express.Router()

router.use('/bar', globalMiddlewareConfigForExpress(fooBarNamespaceMiddlewareFn__userDefined))

const fooBarMiddleware = globalMiddlewareConfigForExpress(fooBarMiddlewareFn__userDefined)

router.get(
  '/foo/bar',
  [auth, ...fooBarMiddleware],
  handleRejection(
    (
      req: Parameters<typeof fooBar__userDefined>[0] & { user: AuthUser },
      res: Parameters<typeof fooBar__userDefined>[1],
    ) => {
      const context = {
        user: req.user,
        entities: {
        },
      }
      return fooBar__userDefined(req, res, context)
    }
  )
)
const fooBazMiddleware = globalMiddlewareConfigForExpress(idFn)

router.get(
  '/foo/baz',
  fooBazMiddleware,
  handleRejection(
    (
      req: Parameters<typeof fooBaz__userDefined>[0],
      res: Parameters<typeof fooBaz__userDefined>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return fooBaz__userDefined(req, res, context)
    }
  )
)

export default router
