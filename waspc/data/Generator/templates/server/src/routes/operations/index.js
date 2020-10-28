{{={= =}=}}
import express from 'express'

{=# isAuthEnabled =}
import auth from '../../core/auth.js'
{=/ isAuthEnabled =}

{=# operationRoutes =}
import {= importIdentifier =} from '{= importPath =}'
{=/ operationRoutes =}

const router = express.Router()

{=# isAuthEnabled =}
router.use(auth)
{=/ isAuthEnabled =}

{=# operationRoutes =}
router.post('{= routePath =}', {= importIdentifier =})
{=/ operationRoutes =}

export default router
