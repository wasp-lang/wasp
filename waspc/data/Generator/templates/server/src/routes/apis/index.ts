{{={= =}=}}
import express from 'express'
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
import { getDefaultMiddleware, toMiddlewareArray } from '../../middleware.js'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
import { type UserInContext } from '../../_types'
{=/ isAuthEnabled =}

const idFn = (x: any) => x

{=# apiRoutes =}
{=& importStatement =}

{=# middlewareConfigFnDefined =}
{=& middlewareImportStatement =}
{=/ middlewareConfigFnDefined =}
{=^ middlewareConfigFnDefined =}
const {=& middlewareImportAlias =} = idFn
{=/ middlewareConfigFnDefined =}

{=/ apiRoutes =}

const router = express.Router()

// TODO: Add check in Haskell for unique API paths.
/*
  NOTE: Since some middleware needs to run _before_ the method, like CORS for GET routes,
  we need to use `Router.use`. However, since this applies to all methods, this means we
  can only support _unique_ route paths.
*/
{=# apiRoutes =}
router.use('{= routePath =}', toMiddlewareArray({= middlewareImportAlias =}(getDefaultMiddleware())))
{=# usesAuth =}
router.{= routeMethod =}('{= routePath =}', auth, handleRejection((req: Parameters<typeof {= importIdentifier =}>[0] & UserInContext, res: Parameters<typeof {= importIdentifier =}>[1]) => {
{=/ usesAuth =}
{=^ usesAuth =}
router.{= routeMethod =}('{= routePath =}', handleRejection((req: Parameters<typeof {= importIdentifier =}>[0], res: Parameters<typeof {= importIdentifier =}>[1]) => {
{=/ usesAuth =}
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
}))
{=/ apiRoutes =}

export default router
