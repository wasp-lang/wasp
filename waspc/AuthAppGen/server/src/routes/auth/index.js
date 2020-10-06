import express from 'express'

import auth from '../../auth.js'
import login from './login.js'
import me from './me.js'

const router = express.Router()

router.post('/login', login)
router.get('/me', auth, me)

export default router
