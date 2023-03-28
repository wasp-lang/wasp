{{={= =}=}}
import express from 'express'
import operations from './operations/index.js'
import { defaultMiddlewareArray } from '../middleware.js'
{=# isAuthEnabled =}
import auth from './auth/index.js'
{=/ isAuthEnabled =}
{=# areThereAnyCustomApiRoutes =}
import apis from './apis/index.js'
{=/ areThereAnyCustomApiRoutes =}


const router = express.Router()

router.get('/', defaultMiddlewareArray, function (_req, res, _next) {
  res.json('Hello world')
})

{=# isAuthEnabled =}
router.use('/auth', defaultMiddlewareArray, auth)
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', defaultMiddlewareArray, operations)
{=# areThereAnyCustomApiRoutes =}
// Keep user-defined api routes last so they cannot override our routes.
router.use(apis)
{=/ areThereAnyCustomApiRoutes =}

export default router
