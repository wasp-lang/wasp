import express from 'express'

import createUser from './createUser.js'
import getUsers from './getUsers.js'

const router = express.Router()

router.post('/create-user', createUser)
router.post('/get-users', getUsers)

export default router
