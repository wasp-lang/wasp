{{={= =}=}}
import express from 'express'

{=# isAuthEnabled =}
import auth from 'wasp/core/auth'
{=/ isAuthEnabled =}

{=# operationRoutes =}
{=& importStatement =}
{=/ operationRoutes =}

const router = express.Router()

{=# operationRoutes =}
router.post('{= routePath =}',{=# isUsingAuth =} auth,{=/ isUsingAuth =} {= importIdentifier =})
{=/ operationRoutes =}

export default router
