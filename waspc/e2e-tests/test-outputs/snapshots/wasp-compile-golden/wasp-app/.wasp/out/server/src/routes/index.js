import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import { config } from 'wasp/server'
import { makeWrongPortPage } from '../views/wrong-port.js'


const waspRouter = express.Router()
const customRouter = express.Router()
const serverRootRouter = express.Router()
const middleware = globalMiddlewareConfigForExpress()

serverRootRouter.get('/', middleware,
    function (_req, res) {
      const data = {
        appName: "waspApp",
        frontendUrl: config.frontendUrl
      };
      const wrongPortPage = makeWrongPortPage(data);
      res.status(200).type('html').send(wrongPortPage);
    }
)

waspRouter.use('/operations', middleware, operations)

export default { waspApi: waspRouter, custom: customRouter, serverRoot: serverRootRouter }
