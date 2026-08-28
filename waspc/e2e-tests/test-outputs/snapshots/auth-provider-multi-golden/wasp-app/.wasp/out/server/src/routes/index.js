import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import auth from './auth/index.js'
import { config } from 'wasp/server'
import { makeWrongPortPage } from '../views/wrong-port.js'


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware,
    function (_req, res) {
      const data = {
        appName: "authProviderMulti",
        frontendUrl: config.frontendUrl
      };
      const wrongPortPage = makeWrongPortPage(data);
      res.status(200).type('html').send(wrongPortPage);
    }
)

router.use('/auth', middleware, auth)
router.use('/operations', middleware, operations)

export default router
