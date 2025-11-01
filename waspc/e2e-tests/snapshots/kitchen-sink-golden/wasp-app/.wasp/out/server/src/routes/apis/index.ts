import express from 'express'
import { prisma } from 'wasp/server'
import { defineHandler } from 'wasp/server/utils'
import { MiddlewareConfigFn, globalMiddlewareConfigForExpress } from '../../middleware/index.js'
import auth from 'wasp/core/auth'
import { type AuthUserData, makeAuthUserIfPossible } from 'wasp/auth/user'

import { barNamespaceMiddlewareFn as _waspbarnamespaceMiddlewareConfigFn } from '../../../../../../src/features/apis/apis'
import { getMiddlewareConfig as _waspdefaultMiddlewareForStreamingTextnamespaceMiddlewareConfigFn } from '../../../../../../src/features/streaming/api'

import { fooBar as _waspfooBarfn } from '../../../../../../src/features/apis/apis'
import { fooBarMiddlewareFn as _waspfooBarmiddlewareConfigFn } from '../../../../../../src/features/apis/apis'
import { barBaz as _waspbarBazfn } from '../../../../../../src/features/apis/apis'
import { webhookCallback as _waspwebhookCallbackfn } from '../../../../../../src/features/apis/apis'
import { webhookCallbackMiddlewareFn as _waspwebhookCallbackmiddlewareConfigFn } from '../../../../../../src/features/apis/apis'
import { getText as _waspstreamingTextfn } from '../../../../../../src/features/streaming/api'

const idFn: MiddlewareConfigFn = x => x

const _waspbarBazmiddlewareConfigFn = idFn
const _waspstreamingTextmiddlewareConfigFn = idFn

const router = express.Router()

router.use('/bar', globalMiddlewareConfigForExpress(_waspbarnamespaceMiddlewareConfigFn))
router.use('/api/streaming-test', globalMiddlewareConfigForExpress(_waspdefaultMiddlewareForStreamingTextnamespaceMiddlewareConfigFn))

const fooBarMiddleware = globalMiddlewareConfigForExpress(_waspfooBarmiddlewareConfigFn)
router.all(
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
          Task: prisma.task,
        },
      }
      return _waspfooBarfn(req, res, context)
    }
  )
)
const barBazMiddleware = globalMiddlewareConfigForExpress(_waspbarBazmiddlewareConfigFn)
router.get(
  '/bar/baz',
  barBazMiddleware,
  defineHandler(
    (
      req: Parameters<typeof _waspbarBazfn>[0],
      res: Parameters<typeof _waspbarBazfn>[1],
    ) => {
      const context = {
        entities: {
          Task: prisma.task,
        },
      }
      return _waspbarBazfn(req, res, context)
    }
  )
)
const webhookCallbackMiddleware = globalMiddlewareConfigForExpress(_waspwebhookCallbackmiddlewareConfigFn)
router.post(
  '/webhook/callback',
  webhookCallbackMiddleware,
  defineHandler(
    (
      req: Parameters<typeof _waspwebhookCallbackfn>[0],
      res: Parameters<typeof _waspwebhookCallbackfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return _waspwebhookCallbackfn(req, res, context)
    }
  )
)
const streamingTextMiddleware = globalMiddlewareConfigForExpress(_waspstreamingTextmiddlewareConfigFn)
router.get(
  '/api/streaming-test',
  [auth, ...streamingTextMiddleware],
  defineHandler(
    (
      req: Parameters<typeof _waspstreamingTextfn>[0] & { user: AuthUserData | null },
      res: Parameters<typeof _waspstreamingTextfn>[1],
    ) => {
      const context = {
        user: makeAuthUserIfPossible(req.user),
        entities: {
        },
      }
      return _waspstreamingTextfn(req, res, context)
    }
  )
)

export default router
