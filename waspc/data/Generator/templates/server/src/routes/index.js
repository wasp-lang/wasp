{{={= =}=}}
import express from 'express'
import operations from './operations/index.js'
{=# isAuthEnabled =}
import auth from './auth/index.js'
{=/ isAuthEnabled =}


const router = express.Router()

router.get('/', function (req, res, next) {
  res.json('Hello world')
})

{=# isAuthEnabled =}
router.use('/auth', auth)
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', operations)

export default router
