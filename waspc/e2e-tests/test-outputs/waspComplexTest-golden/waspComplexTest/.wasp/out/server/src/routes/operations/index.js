import express from 'express'

import auth from 'wasp/core/auth'

import mySpecialAction from './mySpecialAction.js'
import mySpecialQuery from './mySpecialQuery.js'

const router = express.Router()

router.post('/my-special-action', auth, mySpecialAction)
router.post('/my-special-query', auth, mySpecialQuery)

export default router
