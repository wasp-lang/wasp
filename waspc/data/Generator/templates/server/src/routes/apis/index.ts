{{={= =}=}}
import express from 'express'
import { prisma } from 'wasp/server'
import { defineHandler } from 'wasp/server/utils'
import { MiddlewareConfigFn, globalMiddlewareConfigForExpress } from '../../middleware/index.js'
{=# isAuthEnabled =}
import auth from 'wasp/server/core/auth'
import { type AuthUserData, makeAuthUserIfPossible } from 'wasp/auth/user'
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

// Apis and namespaces live under the server base path, unless they set
// `ignoreServerBasePath`, in which case they live at the origin root (see `server.ts`).
const router = express.Router()
const rootRouter = express.Router()

{=# apiNamespaces =}
{= routerName =}.use('{= namespacePath =}', globalMiddlewareConfigForExpress({= namespaceMiddlewareConfigFnImportAlias =}))
{=/ apiNamespaces =}

{=# apiRoutes =}
const {= apiName =}Middleware = globalMiddlewareConfigForExpress({= routeMiddlewareConfigFn.importAlias =})
{= routerName =}.{= routeMethod =}(
  '{= routePath =}',
  {=# usesAuth =}
  [auth, ...{= apiName =}Middleware],
  {=/ usesAuth =}
  {=^ usesAuth =}
  {= apiName =}Middleware,
  {=/ usesAuth =}
  defineHandler(
    (
      req: Parameters<typeof {= importIdentifier =}>[0]{=# usesAuth =} & { user: AuthUserData | null }{=/ usesAuth =},
      res: Parameters<typeof {= importIdentifier =}>[1],
    ) => {
      const context = {
        {=# usesAuth =}
        user: makeAuthUserIfPossible(req.user),
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

export { router, rootRouter }
