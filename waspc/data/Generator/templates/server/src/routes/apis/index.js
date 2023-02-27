{{={= =}=}}
import express from 'express'
import prisma from "../../dbClient.js"

{=# apiRoutes =}
{=& importStatement =}
{=/ apiRoutes =}

const router = express.Router()

{=# apiRoutes =}
// TODO: Add exported types.
router.{= routeVerb =}('{= routePath =}', (req, res, _next) => {
  const context = {
    entities: {
      {=# entities =}
      {= name =}: prisma.{= prismaIdentifier =},
      {=/ entities =}
    },
  }
  {= importIdentifier =}(req, res, context)
})
{=/ apiRoutes =}

export default router
