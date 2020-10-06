import express from 'express'

import auth from '../../auth.js'
import createUser from './createUser.js'
import getUsers from './getUsers.js'

const router = express.Router()

// This route must be public, but it is created by user. So we can't just
// make all operations private because then this one will also be private.
// That means we might need to "know" this is a special signup route, maybe user
// has to tell us that. Or he can explicitly define what he wants public and what private.
router.post('/create-user', createUser)

// Should we make all other "normal" operations private by default?
router.post('/get-users', auth, getUsers)

export default router
