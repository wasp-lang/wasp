{{={= =}=}}
import express from 'express'

import auth from 'wasp/server/core/auth'
import me from './me.js'
import logout from './logout.js'
{=# isExternalAuthProviderUsed =}
import login from './login.js'
{=/ isExternalAuthProviderUsed =}

import providersRouter from '../../auth/providers/index.js'

const router = express.Router()

router.get('/me', auth, me)
router.post('/logout', auth, logout)
{=# isExternalAuthProviderUsed =}
// The credential exchange -- deliberately NOT behind the `auth` middleware,
// since it is the route that establishes the session in the first place.
router.post('/login', login)
{=/ isExternalAuthProviderUsed =}
router.use('/', providersRouter)

export default router
