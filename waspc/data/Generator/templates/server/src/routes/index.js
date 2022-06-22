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

// NOTE: This route, along with all other routes, are CORS-protected and
// restricted to the frontend URL only, preventing CSRF. This helps prevent malicious sites
// from getting a valid token that would be compatible with a logged-in user's cookie.
router.get('/csrf-token', function (req, res) {
  // Added by csurf middleware and creates a token that is validated against the visitor’s CSRF cookie.
  res.json(req.csrfToken())
})
{=/ isAuthEnabled =}
router.use('/{= operationsRouteInRootRouter =}', operations)

export default router
