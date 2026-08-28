{{={= =}=}}
import express from 'express'

{=# isAuthEnabled =}
import auth, { requireSessionProvider } from 'wasp/server/core/auth'
{=/ isAuthEnabled =}

{=# operationRoutes =}
{=& importStatement =}
{=/ operationRoutes =}

const router = express.Router()

{=# operationRoutes =}
router.post('{= routePath =}',{=# isUsingAuth =} auth,{=/ isUsingAuth =}{=# hasRequiredAuthProviderIds =} requireSessionProvider({=& requiredAuthProviderIdsJs =}),{=/ hasRequiredAuthProviderIds =} {= importIdentifier =})
{=/ operationRoutes =}

export default router
