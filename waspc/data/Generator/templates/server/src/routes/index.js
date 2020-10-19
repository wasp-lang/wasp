{{={= =}=}}
import express from 'express'
import operations from './operations/index.js'
import auth from './auth/index.js'


const router = express.Router()

router.get('/', function (req, res, next) {
  res.json('Hello world')
})

router.use('/auth', auth)
router.use('/{= operationsRouteInRootRouter =}', operations)

export default router
