import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareForExpress } from '../middleware/index.js'


const router = express.Router()
const middleware = globalMiddlewareForExpress()

router.get('/', middleware, function (_req, res, _next) {
  res.json('Hello world')
})

router.use('/operations', middleware, operations)

export default router
