{{={= =}=}}
import express, { Request, Response } from 'express'
import prisma from '../../dbClient.js'
import { handleRejection } from '../../utils.js'
{=# isAuthEnabled =}
import auth from '../../core/auth.js'
import { {= userEntityName =}WithoutPassword } from '../../_types'
{=/ isAuthEnabled =}

{=# apiRoutes =}
{=& importStatement =}
{=/ apiRoutes =}

const router = express.Router()

// TODO: How can I use Request/Response instead of any below? Has errors when users override generic types :/
{=# apiRoutes =}
{=# usesAuth =}
router.{= routeMethod =}('{= routePath =}', auth, handleRejection((req: any & { user: {= userEntityName =}WithoutPassword }, res: any) => {
{=/ usesAuth =}
{=^ usesAuth =}
router.{= routeMethod =}('{= routePath =}', handleRejection((req: any, res: any) => {
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
