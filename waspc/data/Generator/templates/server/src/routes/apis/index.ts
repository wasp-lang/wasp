{{={= =}=}}
import express from 'express'
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
import { type SanitizedUser } from '../../_types'
{=/ isAuthEnabled =}

{=# apiRoutes =}
{=& importStatement =}
{=/ apiRoutes =}

const router = express.Router()

{=# apiRoutes =}
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
