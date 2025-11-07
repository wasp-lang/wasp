import express from 'express'
import operations from './operations/index.js'
import { globalMiddlewareConfigForExpress } from '../middleware/index.js'
import { config } from 'wasp/server'


const router = express.Router()
const middleware = globalMiddlewareConfigForExpress()

router.get('/', middleware, function (_req, res) {
  res.render("wrong-port.html.ejs", {
    appName: "waspApp",
    frontendUrl: config.frontendUrl
  });
})

router.use('/operations', middleware, operations)

export default router
