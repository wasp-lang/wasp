import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import auth from './auth/index.js'
import apis from './apis/index.js'
import { rootCrudRouter } from './crud/index.js'
import { config } from 'wasp/server'

const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware, function (_req, res) {
  res.render("wrong-port.html.ejs", {
    appName: "waspComplexTest",
    frontendUrl: config.frontendUrl
  });
})

router.use('/auth', middleware, auth)
router.use('/operations', middleware, operations)
router.use('/crud', middleware, rootCrudRouter)
// NOTE: Keep user-defined api routes last so they cannot override our routes.
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
router.use(apis)

export default router
