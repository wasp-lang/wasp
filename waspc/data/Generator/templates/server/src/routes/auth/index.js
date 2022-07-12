{{={= =}=}}
import express from 'express'

import auth from '../../core/auth.js'
import login from './login.js'
import signup from './signup.js'
import me from './me.js'

{=# isExternalAuthEnabled =}
import passportAuth from './passport/passport.js'
{=/ isExternalAuthEnabled =}

const router = express.Router()

router.post('/login', login)
router.post('/signup', signup)
router.get('/me', auth, me)

{=# isExternalAuthEnabled =}
router.use('/external', passportAuth)
{=/ isExternalAuthEnabled =}

export default router
