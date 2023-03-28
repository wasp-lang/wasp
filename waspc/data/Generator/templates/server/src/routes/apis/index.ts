{{={= =}=}}
import express from 'express'
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
import { defaultMiddleware, toMiddlewareArray } from '../../middleware.js'
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

{=# apiRoutes =}
router.use('{= routePath =}', toMiddlewareArray({= middlewareImportAlias =}(defaultMiddleware())))
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
