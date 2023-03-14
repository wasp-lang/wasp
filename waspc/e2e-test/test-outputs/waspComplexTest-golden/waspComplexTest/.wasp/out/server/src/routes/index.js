import express from 'express'
import operations from './operations/index.js'
import auth from './auth/index.js'
import apis from './apis/index.js'


const router = express.Router()

router.get('/', function (req, res, next) {
  res.json('Hello world')
})

router.use('/auth', auth)
router.use('/operations', operations)
// Keep user-defined api routes last so they cannot override our routes.
router.use(apis)

export default router
