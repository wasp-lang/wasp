import express from 'express'

import login from './login.js'

const router = express.Router()

router.post('/login', login)

export default router
