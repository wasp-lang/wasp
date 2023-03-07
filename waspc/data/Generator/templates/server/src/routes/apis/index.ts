{{={= =}=}}
import express, { Request, Response } from 'express';
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
import { {= userEntityUpper =} } from '../../entities'
{=/ isAuthEnabled =}

{=# apiRoutes =}
{=& importStatement =}
{=/ apiRoutes =}

const router = express.Router()

{=# apiRoutes =}
{=# isUsingAuth =}
router.{= routeMethod =}('{= routePath =}', auth, handleRejection((req: Request & { user: {= userEntityUpper =} }, res: Response) => {
{=/ isUsingAuth =}
{=^ isUsingAuth =}
router.{= routeMethod =}('{= routePath =}', handleRejection((req: Request, res: Response) => {
{=/ isUsingAuth =}
  const context = {
    {=# isUsingAuth =}
    user: req.user,
    {=/ isUsingAuth =}
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
