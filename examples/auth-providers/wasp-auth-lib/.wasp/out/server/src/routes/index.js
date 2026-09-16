import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import auth from './auth/index.js'
import { authProviderRouteHandlers } from 'wasp/server/auth/provider'
import { config } from 'wasp/server'
import { makeWrongPortPage } from '../views/wrong-port.js'


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware,
    function (_req, res) {
      const data = {
        appName: "authProviderWaspAuthLib",
        frontendUrl: config.frontendUrl
      };
      const wrongPortPage = makeWrongPortPage(data);
      res.status(200).type('html').send(wrongPortPage);
    }
)

router.use('/auth', middleware, auth)
// The routes provider 'external:wasp-auth' brought along, mounted where its
// manifest asked. The usual middleware stack applies.
const authProviderMiddleware_0 = globalMiddlewareConfigForExpress((middlewareConfig) => {
  return middlewareConfig
})
router.use('/wasp-auth', authProviderMiddleware_0, (req, res, next) => {
  const routeHandler = authProviderRouteHandlers['external:wasp-auth']
  if (routeHandler === undefined) {
    return next(new Error("The manifest of auth provider 'external:wasp-auth' declares routes, but its server adapter returned no routeHandler."))
  }
  return Promise.resolve(routeHandler(req, res)).catch(next)
})
router.use('/operations', middleware, operations)

export default router
