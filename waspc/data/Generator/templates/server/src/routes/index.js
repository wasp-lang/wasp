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


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware, function (_req, res) {
  {=# isDevelopment =}
  res.json({
    message: 'Wasp Server is up and running!',
    note: 'This is the Wasp server API. If you are looking for the web app, it is running on a separate port (usually 3000).',
    documentation: 'https://wasp-lang.dev/docs'
  });
  {=/ isDevelopment =}
  {=^ isDevelopment =}
  res.status(200).send();
  {=/ isDevelopment =}
})

{=# isAuthEnabled =}
router.use('/auth', middleware, auth)
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', middleware, operations)
{=# areThereAnyCrudRoutes =}
router.use('/{= crudRouteInRootRouter =}', middleware, rootCrudRouter)
{=/ areThereAnyCrudRoutes =}
{=# areThereAnyCustomApiRoutes =}
// NOTE: Keep user-defined api routes last so they cannot override our routes.
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
router.use(apis)
{=/ areThereAnyCustomApiRoutes =}

export default router
