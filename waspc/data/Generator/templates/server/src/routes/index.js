{{={= =}=}}
import express from 'express'
import operations from './operations/index.js'
import { defaultMiddleware, toMiddlewareArray } from '../middleware.js'
{=# isAuthEnabled =}
import auth from './auth/index.js'
{=/ isAuthEnabled =}
{=# areThereAnyCustomApiRoutes =}
import apis from './apis/index.js'
{=/ areThereAnyCustomApiRoutes =}


const router = express.Router()

router.get('/', function (req, res, next) {
  res.json('Hello world')
})

{=# isAuthEnabled =}
router.use('/auth', toMiddlewareArray(defaultMiddleware), auth)
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', toMiddlewareArray(defaultMiddleware), operations)
{=# areThereAnyCustomApiRoutes =}
// Keep user-defined api routes last so they cannot override our routes.
router.use(apis)
{=/ areThereAnyCustomApiRoutes =}

export default router
