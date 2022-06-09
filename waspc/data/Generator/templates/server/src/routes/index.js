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

// NOTE: While we have CORS protection enabled in app.js, it is
// vitally important that this route always be CORS-protected.
router.get('/csrf-token', function (req, res) {
  res.json(req.csrfToken())
})
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', operations)

export default router
