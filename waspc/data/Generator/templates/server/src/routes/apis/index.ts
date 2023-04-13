{{={= =}=}}
import express from 'express'
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
import { globalMiddlewareForExpress } from '../../middleware/index.js'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
import { type SanitizedUser } from '../../_types'
{=/ isAuthEnabled =}

const idFn = (x: any) => x

{=# namespaces =}
{=& namespaceMiddlewareConfigFnImportStatement =}
{=/ namespaces =}

{=# apiRoutes =}
{=& importStatement =}

{=# routeMiddlewareConfigFnDefined =}
{=& routeMiddlewareConfigFnImportStatement =}
{=/ routeMiddlewareConfigFnDefined =}
{=^ routeMiddlewareConfigFnDefined =}
const {=& routeMiddlewareConfigFnImportAlias =} = idFn
{=/ routeMiddlewareConfigFnDefined =}
{=/ apiRoutes =}

const router = express.Router()

{=# namespaces =}
router.use('{= namespacePath =}', globalMiddlewareForExpress({= namespaceMiddlewareConfigFnImportAlias =}))
{=/ namespaces =}

{=# apiRoutes =}
const {= apiName =}Middleware = globalMiddlewareForExpress({= routeMiddlewareConfigFnImportAlias =})
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
