import express from 'express'

import auth from '../../core/auth.js'

import MySpecialAction from './MySpecialAction.js'
import MySpecialQuery from './MySpecialQuery.js'

const router = express.Router()

router.post('/my-special-action', auth, MySpecialAction)
router.post('/my-special-query', auth, MySpecialQuery)

export default router
