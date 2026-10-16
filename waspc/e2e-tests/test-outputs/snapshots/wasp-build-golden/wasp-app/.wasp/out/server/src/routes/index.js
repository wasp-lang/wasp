import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.use('/operations', middleware, operations)
router.get('/up', middleware, function (_req, res) {
  res.status(200).json({ status: 'ok' })
})



export default router
