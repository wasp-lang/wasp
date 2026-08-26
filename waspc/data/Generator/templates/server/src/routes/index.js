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


const waspRouter = express.Router()
const customRouter = express.Router()
const serverRootRouter = express.Router()
const middleware = globalMiddlewareConfigForExpress()

serverRootRouter.get('/', middleware,
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
waspRouter.use('/auth', middleware, auth)
{=/ isAuthEnabled =}
waspRouter.use('/{= operationsRouteInRootRouter =}', middleware, operations)
{=# areThereAnyCrudRoutes =}
waspRouter.use('/{= crudRouteInRootRouter =}', middleware, rootCrudRouter)
{=/ areThereAnyCrudRoutes =}
{=# areThereAnyCustomApiRoutes =}
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
customRouter.use(apis)
{=/ areThereAnyCustomApiRoutes =}

export default { waspApi: waspRouter, custom: customRouter, serverRoot: serverRootRouter }
