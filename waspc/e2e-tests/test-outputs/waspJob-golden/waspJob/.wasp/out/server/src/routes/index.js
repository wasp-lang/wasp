import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware, function (_req, res) {
  res.json({
    message: 'Wasp Server is up and running!',
    note: 'This is the Wasp server API. If you are looking for the web app, it is running on a separate port (usually 3000).',
    documentation: 'https://wasp-lang.dev/docs'
  });
})

router.use('/operations', middleware, operations)

export default router
