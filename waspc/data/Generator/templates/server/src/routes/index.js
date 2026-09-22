{{={= =}=}}
import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
{=# isAuthEnabled =}
import auth from './auth/index.js'
import { runWithRequest } from 'wasp/server/requestContext'
{=/ isAuthEnabled =}
{=# anyAuthProviderRoutes =}
import { authSchemeRouteHandlers } from 'wasp/server/auth/schemes'
import { sendWebResponse, toWebRequest } from 'wasp/server/auth/http'
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

{=# isAuthEnabled =}
// The request binding: everything handling this request, however deep, can
// read it (the auth hooks' `req`, the merge freshness check).
router.use((req, _res, next) => runWithRequest(req, next))
{=/ isAuthEnabled =}

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

{=# authProviderRoutes =}
// The routes scheme '{= schemeName =}' brought along, mounted at
// /auth/{= schemeName =}. BEFORE the framework's own /auth routes below, whose
// middleware stack would parse (and so consume) the body of every /auth/*
// request: the handler gets a standard Request with the raw body stream and
// parses it itself. Scheme names never collide with the framework's routes
// (`me`, `logout` are reserved).
const authProviderMiddleware_{= index =} = globalMiddlewareConfigForExpress((middlewareConfig) => {
  middlewareConfig.delete('express.json')
  middlewareConfig.delete('express.urlencoded')
  return middlewareConfig
})
router.use('{= basePath =}', authProviderMiddleware_{= index =}, (req, res, next) => {
  const routeHandler = authSchemeRouteHandlers['{= schemeName =}']
  if (routeHandler === undefined) {
    return next(new Error("The manifest of auth scheme '{= schemeName =}' declares routes, but its handler returned no routeHandler."))
  }
  return Promise.resolve(routeHandler(toWebRequest(req, { body: true })))
    .then((response) => sendWebResponse(res, response))
    .catch(next)
})
{=/ authProviderRoutes =}
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
