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
{=/ isDevelopment =}

const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware, function (_req, res) {
  const responseText =
    {=# isDevelopment =}
      `
        <div
          style="
            font-family: system-ui, sans-serif;
            width: 90%;
            max-width: 600px;
            margin: 2em auto;
          "
        >
          <h1 style="margin: 0">{= appName =} API Server</h1>
          <p>The server is up and running. This is the backend part of your Wasp application.</p>
          <p>
            If you want to visit your frontend application, go to this URL in your browser:
          </p>
          <a
            href=${JSON.stringify(config.frontendUrl)}
            style="text-align: center; font-size: 1.5em;"
          >
            <pre>${config.frontendUrl}</pre>
          </a>
          <p>
            <small>
              This message is shown because you are running the server in development
              mode. In production, this route would not show anything.
            </small>
          </p>
        </div>
      `;
    {=/ isDevelopment =}
    {=^ isDevelopment =}
      undefined;
    {=/ isDevelopment =}
  res.status(200).send(responseText);
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
