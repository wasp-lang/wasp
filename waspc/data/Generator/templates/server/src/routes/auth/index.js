import express from 'express'

import auth from '../../core/auth.js'
import login from './login.js'
import logout from './logout.js'
import signup from './signup.js'
import me from './me.js'

const router = express.Router()

router.post('/login', login)
router.post('/logout', logout)
router.post('/signup', signup)
router.get('/me', auth, me)

export default router
