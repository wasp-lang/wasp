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
import { rootCrudRouter } from './crud/index.js'


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware, function (_req, res, _next) {
  res.json('Hello world')
})

{=# isAuthEnabled =}
router.use('/auth', middleware, auth)
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', middleware, operations)
router.use('/{= crudRouteInRootRouter =}', middleware, rootCrudRouter)
{=# areThereAnyCustomApiRoutes =}
// NOTE: Keep user-defined api routes last so they cannot override our routes.
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
router.use(apis)
{=/ areThereAnyCustomApiRoutes =}

export default router
