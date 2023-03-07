{{={= =}=}}
import express from 'express'
import operations from './operations/index.js'
{=# isAuthEnabled =}
import auth from './auth/index.js'
{=/ isAuthEnabled =}
{=# apisInUse =}
import apis from './apis/index.js'
{=/ apisInUse =}


const router = express.Router()

router.get('/', function (req, res, next) {
  res.json('Hello world')
})

{=# isAuthEnabled =}
router.use('/auth', auth)
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', operations)
{=# apisInUse =}
// Keep user-defined api routes last so they cannot override our routes.
router.use(apis)
{=/ apisInUse =}

export default router
