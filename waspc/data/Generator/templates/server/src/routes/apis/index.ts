{{={= =}=}}
import express from 'express'
import { prisma } from 'wasp/server'
import { handleRejection } from 'wasp/server/utils'
import { MiddlewareConfigFn, globalMiddlewareConfigForExpress } from '../../middleware/index.js'
{=# isAuthEnabled =}
import auth from 'wasp/core/auth'
import { type SanitizedUser } from 'wasp/server/_types'
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
  handleRejection(
    (
      req: Parameters<typeof {= importIdentifier =}>[0]{=# usesAuth =} & { user: SanitizedUser }{=/ usesAuth =},
      res: Parameters<typeof {= importIdentifier =}>[1],
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
      return {= importIdentifier =}(req, res, context)
    }
  )
)
{=/ apiRoutes =}

export default router
