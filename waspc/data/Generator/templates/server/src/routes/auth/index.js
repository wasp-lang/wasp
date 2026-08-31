{{={= =}=}}
import express from 'express'

import auth from 'wasp/server/core/auth'
import me from './me.js'
import logout from './logout.js'

import providersRouter from '../../auth/providers/index.js'

const router = express.Router()

router.get('/me', auth, me)
router.post('/logout', logout)
router.use('/', providersRouter)

export default router
