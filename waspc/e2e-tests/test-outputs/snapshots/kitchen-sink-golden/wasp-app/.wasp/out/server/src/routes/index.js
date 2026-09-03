import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import auth from './auth/index.js'
import { authProviderRouteHandlers } from 'wasp/server/auth/provider'
import apis from './apis/index.js'
import { rootCrudRouter } from './crud/index.js'
import { config } from 'wasp/server'
import { makeWrongPortPage } from '../views/wrong-port.js'


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware,
    function (_req, res) {
      const data = {
        appName: "KitchenSink",
        frontendUrl: config.frontendUrl
      };
      const wrongPortPage = makeWrongPortPage(data);
      res.status(200).type('html').send(wrongPortPage);
    }
)

router.use('/auth', middleware, auth)
// The routes provider 'wasp' brought along, mounted where its
// manifest asked (Wasp's own auth at /auth/wasp, after the framework's own
// /auth routes above). The usual middleware stack applies.
const authProviderMiddleware_0 = globalMiddlewareConfigForExpress((middlewareConfig) => {
  return middlewareConfig
})
router.use('/auth/wasp', authProviderMiddleware_0, (req, res, next) => {
  const routeHandler = authProviderRouteHandlers['wasp']
  if (routeHandler === undefined) {
    return next(new Error("The manifest of auth provider 'wasp' declares routes, but its server adapter returned no routeHandler."))
  }
  return Promise.resolve(routeHandler(req, res)).catch(next)
})
router.use('/operations', middleware, operations)
router.use('/crud', middleware, rootCrudRouter)
// NOTE: Keep user-defined api routes last so they cannot override our routes.
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
router.use(apis)

export default router
