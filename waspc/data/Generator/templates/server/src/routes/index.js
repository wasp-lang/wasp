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
{=# isDevelopment =}
import { config } from 'wasp/server'
import { makeWrongPortPage } from '../views/wrong-port.js'
{=/ isDevelopment =}


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

{=# isAuthEnabled =}
router.use('/{= authRouteInRootRouter =}', middleware, auth)
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', middleware, operations)
{=# areThereAnyCrudRoutes =}
router.use('/{= crudRouteInRootRouter =}', middleware, rootCrudRouter)
{=/ areThereAnyCrudRoutes =}
router.get('/{= healthRouteInRootRouter =}', middleware, function (_req, res) {
  res.status(200).json({ status: 'ok' })
})

{=# areThereAnyCustomApiRoutes =}
// NOTE: Keep user-defined api routes after Wasp's routes above so they cannot
// override them. Additionally, do not add middleware to these routes here.
// Instead, we add it later to allow for middleware customization.
router.use(apis)
{=/ areThereAnyCustomApiRoutes =}

{=# isDevelopment =}
// Registered last, so an `api` at `/` answers instead of this page.
// You normally reach this page only on the server's own port, since the client dev
// server proxies `/` only when an `api` claims the root.
router.get('/', middleware, function (_req, res) {
  const data = {
    appName: "{= appName =}",
    frontendUrl: config.frontendUrl,
    deploymentMode: "{= deploymentMode =}",
  };
  const wrongPortPage = makeWrongPortPage(data);
  res.status(200).type('html').send(wrongPortPage);
})
{=/ isDevelopment =}

export default router
