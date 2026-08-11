{{={= =}=}}
import express from 'express'
import { prisma } from 'wasp/server'
import { defineHandler } from 'wasp/server/utils'
import { MiddlewareConfigFn, globalMiddlewareConfigForExpress } from '../../middleware/index.js'
{=# isAuthEnabled =}
import auth from 'wasp/server/core/auth'
import { makeAuthUserIfPossible } from 'wasp/auth/user'
{=/ isAuthEnabled =}

{=# apiNamespaces =}
{=& namespaceMiddlewareConfigFnImportStatement =}
{=/ apiNamespaces =}

{=# apiRoutes =}
{=& importStatement =}
{=# routeMiddlewareConfigFn.isDefined =}
{=& routeMiddlewareConfigFn.importStatement =}
{=/ routeMiddlewareConfigFn.isDefined =}
{=/ apiRoutes =}

const idFn: MiddlewareConfigFn = x => x

{=# apiRoutes =}
{=^ routeMiddlewareConfigFn.isDefined =}
const {=& routeMiddlewareConfigFn.importAlias =} = idFn
{=/ routeMiddlewareConfigFn.isDefined =}
{=/ apiRoutes =}

const router = express.Router()

{=# apiNamespaces =}
router.use('{= namespacePath =}', globalMiddlewareConfigForExpress({= namespaceMiddlewareConfigFnImportAlias =}))
{=/ apiNamespaces =}

{=# apiRoutes =}
const {= apiName =}Middleware = globalMiddlewareConfigForExpress({= routeMiddlewareConfigFn.importAlias =})
router.{= routeMethod =}(
  '{= routePath =}',
  {=# usesAuth =}
  [auth, ...{= apiName =}Middleware],
  {=/ usesAuth =}
  {=^ usesAuth =}
  {= apiName =}Middleware,
  {=/ usesAuth =}
  defineHandler(
    (
      req: Parameters<typeof {= importIdentifier =}>[0],
      res: Parameters<typeof {= importIdentifier =}>[1],
    ) => {
      const context = {
        {=# usesAuth =}
        // `req.user` is declared by our global Express Request augmentation
        // (see `wasp/server/utils`), where it is optional. The `auth`
        // middleware has already rejected the request if there is no user, and
        // `ContextWithUser` declares `user` as optional, not nullable.
        user: makeAuthUserIfPossible(req.user ?? null) ?? undefined,
        {=/ usesAuth =}
        entities: {
          {=# entities =}
          {= name =}: prisma.{= prismaIdentifier =},
          {=/ entities =}
        },
      }
      return {= importIdentifier =}(req, res, context)
    }
  )
)
{=/ apiRoutes =}

export default router
