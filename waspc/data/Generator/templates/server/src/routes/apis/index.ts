{{={= =}=}}
import express from 'express'
import { prisma } from 'wasp/server'
import { handleRejection } from 'wasp/server/utils'
import { MiddlewareConfigFn, globalMiddlewareConfigForExpress } from '../../middleware/index.js'
{=# isAuthEnabled =}
import auth from 'wasp/core/auth'
import { type AuthUser } from 'wasp/auth'
{=/ isAuthEnabled =}

{=# apiNamespaces =}
{=& namespaceMiddlewareConfigFn.importStatement =}
{=/ apiNamespaces =}

{=# apiRoutes =}
{=& apiRouteFn.importStatement =}
{=# routeMiddlewareConfigFn.isDefined =}
{=& routeMiddlewareConfigFn.importStatement =}
{=/ routeMiddlewareConfigFn.isDefined =}
{=/ apiRoutes =}

const idFn: MiddlewareConfigFn = x => x

const router = express.Router()

{=# apiNamespaces =}
router.use('{= namespacePath =}', globalMiddlewareConfigForExpress({= namespaceMiddlewareConfigFn.importIdentifier =}))
{=/ apiNamespaces =}

{=# apiRoutes =}
{=# routeMiddlewareConfigFn.isDefined =}
const {= apiName =}Middleware = globalMiddlewareConfigForExpress({= routeMiddlewareConfigFn.importIdentifier =})
{=/ routeMiddlewareConfigFn.isDefined =}
{=^ routeMiddlewareConfigFn.isDefined =}
const {= apiName =}Middleware = globalMiddlewareConfigForExpress(idFn)
{=/ routeMiddlewareConfigFn.isDefined =}

router.{= routeMethod =}(
  '{= routePath =}',
  {=# usesAuth =}
  [auth, ...{= apiName =}Middleware],
  {=/ usesAuth =}
  {=^ usesAuth =}
  {= apiName =}Middleware,
  {=/ usesAuth =}
  handleRejection(
    (
      req: Parameters<typeof {= apiRouteFn.importIdentifier =}>[0]{=# usesAuth =} & { user: AuthUser }{=/ usesAuth =},
      res: Parameters<typeof {= apiRouteFn.importIdentifier =}>[1],
    ) => {
      const context = {
        {=# usesAuth =}
        user: req.user,
        {=/ usesAuth =}
        entities: {
          {=# entities =}
          {= name =}: prisma.{= prismaIdentifier =},
          {=/ entities =}
        },
      }
      return {= apiRouteFn.importIdentifier =}(req, res, context)
    }
  )
)
{=/ apiRoutes =}

export default router
