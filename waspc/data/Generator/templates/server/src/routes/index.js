{{={= =}=}}
import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
{=# isAuthEnabled =}
import auth from './auth/index.js'
{=/ isAuthEnabled =}
{=# anyAuthProviderRoutes =}
import { authSchemeRouteHandlers } from 'wasp/server/auth/schemes'
{=/ anyAuthProviderRoutes =}
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

router.get('/', middleware,
  {=# isDevelopment =}
    function (_req, res) {
      const data = {
        appName: "{= appName =}",
        frontendUrl: config.frontendUrl
      };
      const wrongPortPage = makeWrongPortPage(data);
      res.status(200).type('html').send(wrongPortPage);
    }
  {=/ isDevelopment =}
  {=^ isDevelopment =}
    function (_req, res) {
      res.status(200).send();
    }
  {=/ isDevelopment =}
)

{=# isAuthEnabled =}
router.use('/auth', middleware, auth)
{=/ isAuthEnabled =}
{=# authProviderRoutes =}
// The routes scheme '{= schemeName =}' brought along, mounted at
// /auth/{= schemeName =}, after the framework's own /auth routes above. The
// usual middleware stack applies{=# rawBody =}, minus the
// body parsers: the provider's handler reads the raw request stream itself,
// and a body that was already consumed would make every request to it
// hang{=/ rawBody =}.
const authProviderMiddleware_{= index =} = globalMiddlewareConfigForExpress((middlewareConfig) => {
  {=# rawBody =}
  middlewareConfig.delete('express.json')
  middlewareConfig.delete('express.urlencoded')
  {=/ rawBody =}
  return middlewareConfig
})
router.use('{= basePath =}', authProviderMiddleware_{= index =}, (req, res, next) => {
  const routeHandler = authSchemeRouteHandlers['{= schemeName =}']
  if (routeHandler === undefined) {
    return next(new Error("The manifest of auth scheme '{= schemeName =}' declares routes, but its handler returned no routeHandler."))
  }
  return Promise.resolve(routeHandler(req, res)).catch(next)
})
{=/ authProviderRoutes =}
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
