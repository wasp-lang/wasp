{{={= =}=}}
import express from 'express'

{=# queryRoutes =}
import {= importIdentifier =} from '{= importPath =}'
{=/ queryRoutes =}

const router = express.Router()

{=# queryRoutes =}
router.post('{= routePath =}', {= importIdentifier =})
{=/ queryRoutes =}

export default router
