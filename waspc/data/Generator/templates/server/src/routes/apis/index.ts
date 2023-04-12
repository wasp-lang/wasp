{{={= =}=}}
import express from 'express'
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
import { getDefaultMiddleware, toMiddlewareArray } from '../../middleware.js'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
import { type SanitizedUser } from '../../_types'
{=/ isAuthEnabled =}

const idFn = (x: any) => x

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

/*
  NOTE: Since some middleware needs to run _before_ the method, like CORS for GET/POST requests,
  we need to use `Router.use`. However, since this applies to all methods for that route, this
  means we can only support _unique_ route paths.
*/
{=# apiRoutes =}
router.use('{= routePath =}', toMiddlewareArray({= routeMiddlewareConfigFnImportAlias =}(getDefaultMiddleware())))
router.{= routeMethod =}(
  '{= routePath =}',
  {=# usesAuth =}
  auth,
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
