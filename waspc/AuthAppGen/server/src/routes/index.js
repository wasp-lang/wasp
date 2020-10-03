import express from 'express'
import operations from './operations/index.js'

// Added by Matija
import auth from './auth/index.js'

const router = express.Router()

router.get('/', function (req, res, next) {
  res.json('Hello world')
})

router.use('/queries', operations)
router.use ('/auth', auth)

export default router
