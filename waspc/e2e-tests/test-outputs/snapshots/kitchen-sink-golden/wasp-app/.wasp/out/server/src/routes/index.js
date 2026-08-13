import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import auth from './auth/index.js'
import apis from './apis/index.js'
import { rootCrudRouter } from './crud/index.js'


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
router.use('/auth', middleware, auth, respondWithNotFound)
router.use('/operations', middleware, operations, respondWithNotFound)
router.use('/crud', middleware, rootCrudRouter, respondWithNotFound)
// NOTE: Keep user-defined api routes last so they cannot override our routes.
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
//
// Requests these routes don't match fall through to the app's pages, because we
// can't tell a mistyped custom API path from a page's path.
router.use(apis)

function respondWithNotFound(_req, res) {
  res.status(404).json({ message: 'Not Found' })
}

export default router
