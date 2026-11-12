{{={= =}=}}
import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
{=# isAuthEnabled =}
import auth from './auth/index.js'
{=/ isAuthEnabled =}
{=# areThereAnyCustomApiRoutes =}
import apis from './apis/index.js'
{=/ areThereAnyCustomApiRoutes =}
{=# areThereAnyCrudRoutes =}
import { rootCrudRouter } from './crud/index.js'
{=/ areThereAnyCrudRoutes =}
{=# isDevMailboxEnabled =}
import { devMailbox, devMailboxRoutePath, registerDevMailboxRoutes } from 'wasp/server/email/core/devMailbox'
{=/ isDevMailboxEnabled =}
{=# isDevelopment =}
import { config } from 'wasp/server'
import { makeWrongPortPage } from '../views/wrong-port.js'
{=/ isDevelopment =}


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

{=# isAuthEnabled =}
router.use('/auth', middleware, auth)
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', middleware, operations)
{=# areThereAnyCrudRoutes =}
router.use('/{= crudRouteInRootRouter =}', middleware, rootCrudRouter)
{=/ areThereAnyCrudRoutes =}
router.get('/{= upRouteInRootRouter =}', middleware, function (_req, res) {
  res.status(200).json({ status: 'ok' })
})

{=# isDevMailboxEnabled =}
// The inbox and API for emails sent with the Dummy email sender. Without the
// global middleware, since it isn't part of the app's API.
const devMailboxRouter = express.Router()
devMailboxRouter.use(express.json())
registerDevMailboxRoutes(devMailboxRouter, devMailbox, { appName: "{= appName =}" })
router.use(devMailboxRoutePath, devMailboxRouter)

{=/ isDevMailboxEnabled =}
{=# areThereAnyCustomApiRoutes =}
// NOTE: Keep user-defined api routes last so they cannot override our routes.
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
router.use(apis)
{=/ areThereAnyCustomApiRoutes =}

{=# isDevelopment =}
// A page for people who open the server's URL in the browser by mistake.
// Registered last, so an `api` at `/` answers instead of it, and without the
// global middleware, since it's only ever read by a human in a browser.
router.get('/', function (_req, res) {
  const data = {
    appName: "{= appName =}",
    frontendUrl: config.frontendUrl
  };
  const wrongPortPage = makeWrongPortPage(data);
  res.status(200).type('html').send(wrongPortPage);
})
{=/ isDevelopment =}

export default router
