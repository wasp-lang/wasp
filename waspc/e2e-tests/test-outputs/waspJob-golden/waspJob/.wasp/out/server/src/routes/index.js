import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
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
          <h1 style="margin: 0">waspJob API Server</h1>
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

router.use('/operations', middleware, operations)

export default router
