{{={= =}=}}
import express from 'express'

import auth from '../../core/auth.js'

{=# operationRoutes =}
import {= importIdentifier =} from '{= importPath =}'
{=/ operationRoutes =}

const router = express.Router()

router.use(auth)

{=# operationRoutes =}
router.post('{= routePath =}', {= importIdentifier =})
{=/ operationRoutes =}

export default router
