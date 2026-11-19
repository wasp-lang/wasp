import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import auth from './auth/index.js'
import apis from './apis/index.js'
import { rootCrudRouter } from './crud/index.js'
import { config } from 'wasp/server'
import { makeWrongPortPage } from '../views/wrong-port.js'
import cors from 'cors'
import { decls } from './spec.js'


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.use('/auth', middleware, auth)
router.use('/operations', middleware, operations)
router.use('/crud', middleware, rootCrudRouter)
router.get('/up', middleware, function (_req, res) {
  res.status(200).json({ status: 'ok' })
})

// The app's declarations, for Wasp Studio. Only in development, since they
// describe the app's internals. Instead of the global middleware, it allows
// the Wasp Studio page as well as the client to read it.
router.get(
  '/__wasp/spec',
  cors({ origin: [config.frontendUrl, 'http://localhost:4000'] }),
  function (_req, res) {
    res.status(200).json(decls)
  }
)

// NOTE: Keep user-defined api routes last so they cannot override our routes.
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
router.use(apis)

// A page for people who open the server's URL in the browser by mistake.
// Registered last, so an `api` at `/` answers instead of it, and without the
// global middleware, since it's only ever read by a human in a browser.
router.get('/', function (_req, res) {
  const data = {
    appName: "KitchenSink",
    frontendUrl: config.frontendUrl
  };
  const wrongPortPage = makeWrongPortPage(data);
  res.status(200).type('html').send(wrongPortPage);
})

export default router
