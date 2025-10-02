import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import auth from './auth/index.js'
import apis from './apis/index.js'
import { rootCrudRouter } from './crud/index.js'
import { config } from 'wasp/server'

const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware, function (_req, res) {
  const responseText =
      `
        <div
          style="
            font-family: system-ui, sans-serif;
            width: 90%;
            max-width: 600px;
            margin: 2em auto;
          "
        >
          <h1 style="margin: 0">waspComplexTest API Server</h1>
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
  res.status(200).send(responseText);
})

router.use('/auth', middleware, auth)
router.use('/operations', middleware, operations)
router.use('/crud', middleware, rootCrudRouter)
// NOTE: Keep user-defined api routes last so they cannot override our routes.
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
router.use(apis)

export default router
