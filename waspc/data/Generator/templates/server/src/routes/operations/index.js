{{={= =}=}}
import express from 'express'

{=# operationRoutes =}
import {= importIdentifier =} from '{= importPath =}'
{=/ operationRoutes =}

const router = express.Router()

{=# operationRoutes =}
router.post('{= routePath =}', {= importIdentifier =})
{=/ operationRoutes =}

export default router
