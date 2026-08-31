import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'


const waspRouter = express.Router()
const customRouter = express.Router()
const serverRootRouter = express.Router()
const middleware = globalMiddlewareConfigForExpress()

serverRootRouter.get('/', middleware,
    function (_req, res) {
      res.status(200).send();
    }
)

waspRouter.use('/operations', middleware, operations)

export default { waspApi: waspRouter, custom: customRouter, serverRoot: serverRootRouter }
