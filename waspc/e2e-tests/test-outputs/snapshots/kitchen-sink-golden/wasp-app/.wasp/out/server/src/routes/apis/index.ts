import express from 'express'
import { prisma } from 'wasp/server'
import { defineHandler } from 'wasp/server/utils'
import { MiddlewareConfigFn, globalMiddlewareConfigForExpress } from '../../middleware/index.js'
import auth from 'wasp/server/core/auth'
import { type AuthUserData, makeAuthUserIfPossible } from 'wasp/auth/user'

import { barNamespaceMiddlewareFn as _waspbarNamespaceMiddlewareFnnamespaceMiddlewareConfigFn } from "../../../../../../src/features/apis/apis"
import { defaultMiddlewareForStreamingText as _waspdefaultMiddlewareForStreamingTextnamespaceMiddlewareConfigFn } from "../../../../../../src/features/streaming/api"

import { fooBar as _waspfooBarfn } from "../../../../../../src/features/apis/apis"
import { fooBarMiddlewareFn as _waspfooBarmiddlewareConfigFn } from "../../../../../../src/features/apis/apis"
import { headBarBaz as _waspheadBarBazfn } from "../../../../../../src/features/apis/apis"
import { barBaz as _waspbarBazfn } from "../../../../../../src/features/apis/apis"
import { patchBarBaz as _wasppatchBarBazfn } from "../../../../../../src/features/apis/apis"
import { webhookCallback as _waspwebhookCallbackfn } from "../../../../../../src/features/apis/apis"
import { webhookCallbackMiddlewareFn as _waspwebhookCallbackmiddlewareConfigFn } from "../../../../../../src/features/apis/apis"
import { throwUnexpectedError as _waspthrowUnexpectedErrorfn } from "../../../../../../src/features/errors/apis"
import { throwHttpError as _waspthrowHttpErrorfn } from "../../../../../../src/features/errors/apis"
import { throwConcealedError as _waspthrowConcealedErrorfn } from "../../../../../../src/features/errors/apis"
import { throwUnavailableError as _waspthrowUnavailableErrorfn } from "../../../../../../src/features/errors/apis"
import { throwRateLimitError as _waspthrowRateLimitErrorfn } from "../../../../../../src/features/errors/apis"
import { streamingText as _waspstreamingTextfn } from "../../../../../../src/features/streaming/api"

const idFn: MiddlewareConfigFn = x => x

const _waspheadBarBazmiddlewareConfigFn = idFn
const _waspbarBazmiddlewareConfigFn = idFn
const _wasppatchBarBazmiddlewareConfigFn = idFn
const _waspthrowUnexpectedErrormiddlewareConfigFn = idFn
const _waspthrowHttpErrormiddlewareConfigFn = idFn
const _waspthrowConcealedErrormiddlewareConfigFn = idFn
const _waspthrowUnavailableErrormiddlewareConfigFn = idFn
const _waspthrowRateLimitErrormiddlewareConfigFn = idFn
const _waspstreamingTextmiddlewareConfigFn = idFn

const router = express.Router()

router.use("/bar", globalMiddlewareConfigForExpress(_waspbarNamespaceMiddlewareFnnamespaceMiddlewareConfigFn))
router.use("/api/streaming-test", globalMiddlewareConfigForExpress(_waspdefaultMiddlewareForStreamingTextnamespaceMiddlewareConfigFn))

const fooBarMiddleware = globalMiddlewareConfigForExpress(_waspfooBarmiddlewareConfigFn)
router.all(
  "/foo/bar",
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
const headBarBazMiddleware = globalMiddlewareConfigForExpress(_waspheadBarBazmiddlewareConfigFn)
router.head(
  "/bar/baz",
  headBarBazMiddleware,
  defineHandler(
    (
      req: Parameters<typeof _waspheadBarBazfn>[0],
      res: Parameters<typeof _waspheadBarBazfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return _waspheadBarBazfn(req, res, context)
    }
  )
)
const barBazMiddleware = globalMiddlewareConfigForExpress(_waspbarBazmiddlewareConfigFn)
router.get(
  "/bar/baz",
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
const patchBarBazMiddleware = globalMiddlewareConfigForExpress(_wasppatchBarBazmiddlewareConfigFn)
router.patch(
  "/bar/baz",
  patchBarBazMiddleware,
  defineHandler(
    (
      req: Parameters<typeof _wasppatchBarBazfn>[0],
      res: Parameters<typeof _wasppatchBarBazfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return _wasppatchBarBazfn(req, res, context)
    }
  )
)
const webhookCallbackMiddleware = globalMiddlewareConfigForExpress(_waspwebhookCallbackmiddlewareConfigFn)
router.post(
  "/webhook/callback",
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
const throwUnexpectedErrorMiddleware = globalMiddlewareConfigForExpress(_waspthrowUnexpectedErrormiddlewareConfigFn)
router.get(
  "/errors/unexpected",
  throwUnexpectedErrorMiddleware,
  defineHandler(
    (
      req: Parameters<typeof _waspthrowUnexpectedErrorfn>[0],
      res: Parameters<typeof _waspthrowUnexpectedErrorfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return _waspthrowUnexpectedErrorfn(req, res, context)
    }
  )
)
const throwHttpErrorMiddleware = globalMiddlewareConfigForExpress(_waspthrowHttpErrormiddlewareConfigFn)
router.get(
  "/errors/http",
  throwHttpErrorMiddleware,
  defineHandler(
    (
      req: Parameters<typeof _waspthrowHttpErrorfn>[0],
      res: Parameters<typeof _waspthrowHttpErrorfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return _waspthrowHttpErrorfn(req, res, context)
    }
  )
)
const throwConcealedErrorMiddleware = globalMiddlewareConfigForExpress(_waspthrowConcealedErrormiddlewareConfigFn)
router.get(
  "/errors/concealed",
  throwConcealedErrorMiddleware,
  defineHandler(
    (
      req: Parameters<typeof _waspthrowConcealedErrorfn>[0],
      res: Parameters<typeof _waspthrowConcealedErrorfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return _waspthrowConcealedErrorfn(req, res, context)
    }
  )
)
const throwUnavailableErrorMiddleware = globalMiddlewareConfigForExpress(_waspthrowUnavailableErrormiddlewareConfigFn)
router.get(
  "/errors/unavailable",
  throwUnavailableErrorMiddleware,
  defineHandler(
    (
      req: Parameters<typeof _waspthrowUnavailableErrorfn>[0],
      res: Parameters<typeof _waspthrowUnavailableErrorfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return _waspthrowUnavailableErrorfn(req, res, context)
    }
  )
)
const throwRateLimitErrorMiddleware = globalMiddlewareConfigForExpress(_waspthrowRateLimitErrormiddlewareConfigFn)
router.get(
  "/errors/rate-limit",
  throwRateLimitErrorMiddleware,
  defineHandler(
    (
      req: Parameters<typeof _waspthrowRateLimitErrorfn>[0],
      res: Parameters<typeof _waspthrowRateLimitErrorfn>[1],
    ) => {
      const context = {
        entities: {
        },
      }
      return _waspthrowRateLimitErrorfn(req, res, context)
    }
  )
)
const streamingTextMiddleware = globalMiddlewareConfigForExpress(_waspstreamingTextmiddlewareConfigFn)
router.get(
  "/api/streaming-test",
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
