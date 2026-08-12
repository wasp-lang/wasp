import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

// A route deployments can use to check that the server is up. Everything else
// under `/` belongs to the app's pages.
router.get('/_wasp/health', middleware, function (_req, res) {
  res.status(200).json({ status: 'ok' })
})

// NOTE: Each of Wasp's own routers ends with `respondWithNotFound`, so that a
// request for one of them that no route matches (a typo in an operation's name,
// say) gets an honest 404 instead of falling through to the app's pages.
router.use('/operations', middleware, operations, respondWithNotFound)

function respondWithNotFound(_req, res) {
  res.status(404).json({ message: 'Not Found' })
}

export default router
