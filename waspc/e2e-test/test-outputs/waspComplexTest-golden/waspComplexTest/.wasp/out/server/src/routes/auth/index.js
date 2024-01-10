import express from 'express'

import auth from '../../core/auth.js'
import me from './me.js'
import logout from './logout.js'

import providersRouter from '../../auth/providers/index.js'

const router = express.Router()

router.get('/me', auth, me)
router.post('/logout', auth, logout)
router.use('/', providersRouter)

export default router
