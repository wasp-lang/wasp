import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import auth from './auth/index.js'
import apis from './apis/index.js'
import { rootCrudRouter } from './crud/index.js'
import { config } from 'wasp/server'
import { makeWrongPortPage } from '../views/wrong-port.js'


const waspRouter = express.Router()
const customRouter = express.Router()
const serverRootRouter = express.Router()
const middleware = globalMiddlewareConfigForExpress()

serverRootRouter.get('/', middleware,
    function (_req, res) {
      const data = {
        appName: "KitchenSink",
        frontendUrl: config.frontendUrl
      };
      const wrongPortPage = makeWrongPortPage(data);
      res.status(200).type('html').send(wrongPortPage);
    }
)

waspRouter.use('/auth', middleware, auth)
waspRouter.use('/operations', middleware, operations)
waspRouter.use('/crud', middleware, rootCrudRouter)
// Additionally, do not add middleware to these routes here. Instead, we add
// it later to allow for middleware customization.
customRouter.use(apis)

export default { waspApi: waspRouter, custom: customRouter, serverRoot: serverRootRouter }
