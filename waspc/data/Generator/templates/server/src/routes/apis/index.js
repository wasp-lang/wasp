{{={= =}=}}
import express from 'express'

{=# apiRoutes =}
{=& importStatement =}
{=/ apiRoutes =}

const router = express.Router()

{=# apiRoutes =}
router.{= routeVerb =}('{= routePath =}', {= importIdentifier =})
{=/ apiRoutes =}

export default router
