{{={= =}=}}
import express from 'express'

import auth from '../../core/auth.js'
import me from './me.js'

{=# isExternalAuthEnabled =}
import providersRouter from './providers/index.js'
{=/ isExternalAuthEnabled =}

const router = express.Router()

router.get('/me', auth, me)

{=# isExternalAuthEnabled =}
router.use('/', providersRouter)
{=/ isExternalAuthEnabled =}

export default router
