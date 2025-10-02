import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'

const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware, function (_req, res) {
  const responseText =
      undefined;
  res.status(200).send(responseText);
})

router.use('/operations', middleware, operations)

export default router
