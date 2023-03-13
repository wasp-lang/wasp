{{={= =}=}}
import express from 'express'
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
import { type UserInContext } from '../../_types'
{=/ isAuthEnabled =}

{=# apiRoutes =}
{=& importStatement =}
{=/ apiRoutes =}

const router = express.Router()

{=# apiRoutes =}
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
