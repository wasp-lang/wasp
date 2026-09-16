{{={= =}=}}
import express from 'express'

{=# isAuthEnabled =}
import auth, { requireSchemes } from 'wasp/server/core/auth'
{=/ isAuthEnabled =}

{=# operationRoutes =}
{=& importStatement =}
{=/ operationRoutes =}

const router = express.Router()

{=# operationRoutes =}
router.post('{= routePath =}',{=# isUsingAuth =} auth,{=/ isUsingAuth =}{=# hasRequiredAuthProviderIds =} requireSchemes({=& requiredAuthProviderIdsJs =}),{=/ hasRequiredAuthProviderIds =} {= importIdentifier =})
{=/ operationRoutes =}

export default router
