import express from 'express'

import auth from '../../auth.js'
import createUser from './createUser.js'
import getUsers from './getUsers.js'

const router = express.Router()

// All operations are private.
router.use(auth)

router.post('/create-user', createUser)
router.post('/get-users', getUsers)

export default router
