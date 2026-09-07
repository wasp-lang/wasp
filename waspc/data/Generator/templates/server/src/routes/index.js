{{={= =}=}}
import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
{=# isAuthEnabled =}
import auth from './auth/index.js'
{=/ isAuthEnabled =}
{=# areThereAnyCustomApiRoutes =}
import { router as apis } from './apis/index.js'
{=/ areThereAnyCustomApiRoutes =}
{=# areThereAnyCrudRoutes =}
import { rootCrudRouter } from './crud/index.js'
{=/ areThereAnyCrudRoutes =}

// Everything here is mounted under the server base path (see `server.ts`).
const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

// Wasp's own routes.
const waspRouter = express.Router()
{=# isAuthEnabled =}
waspRouter.use('/auth', middleware, auth)
{=/ isAuthEnabled =}
waspRouter.use('/{= operationsRouteInWaspRouter =}', middleware, operations)
{=# areThereAnyCrudRoutes =}
waspRouter.use('/{= crudRouteInWaspRouter =}', middleware, rootCrudRouter)
{=/ areThereAnyCrudRoutes =}

router.use(waspRouter)
{=# areThereAnyCustomApiRoutes =}
// NOTE: Keep user-defined api routes after ours so they cannot override our routes.
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
router.use(apis)
{=/ areThereAnyCustomApiRoutes =}

export default router
