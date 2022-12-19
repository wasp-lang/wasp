{{={= =}=}}
import express from 'express'

{=# isAuthEnabled =}
import auth from '../../core/auth.js'
{=/ isAuthEnabled =}

{=# operationRoutes =}
import {= importIdentifier =} from '{= importPath =}'
{=/ operationRoutes =}

const router = express.Router()

{=# operationRoutes =}
{=# isQuery =}
router.get('{= routePath =}', {=# isUsingAuth =} auth, {=/ isUsingAuth =} {= importIdentifier =})
{=/ isQuery =}
{=^ isQuery =}
router.post('{= routePath =}', {=# isUsingAuth =} auth, {=/ isUsingAuth =} {= importIdentifier =})
{=/ isQuery =}
{=/ operationRoutes =}

export default router
