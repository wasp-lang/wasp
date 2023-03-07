{{={= =}=}}
import express, { Request, Response } from 'express';
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'

{=# apiRoutes =}
{=& importStatement =}
{=/ apiRoutes =}

const router = express.Router()

{=# apiRoutes =}
router.{= routeMethod =}('{= routePath =}', handleRejection((req: Request, res: Response) => {
  const context = {
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
