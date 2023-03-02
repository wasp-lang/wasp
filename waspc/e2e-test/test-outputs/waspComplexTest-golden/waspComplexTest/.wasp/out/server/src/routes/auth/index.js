import express from 'express'

import auth from '../../core/auth.js'
import login from './login.js'
import signup from './signup.js'
import me from './me.js'

import passportAuth from './passport/passport.js'

const router = express.Router()

router.post('/login', login)
router.post('/signup', signup)
router.get('/me', auth, me)

router.use('/external', passportAuth)

export default router
