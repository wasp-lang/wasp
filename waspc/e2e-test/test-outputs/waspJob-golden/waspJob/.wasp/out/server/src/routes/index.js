import express from 'express'
import operations from './operations/index.js'


const router = express.Router()

router.get('/', function (req, res, next) {
  res.json('Hello world')
})

router.use('/operations', operations)

export default router
