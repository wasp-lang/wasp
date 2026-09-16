import express from 'express'

import auth from 'wasp/server/core/auth'
import me from './me.js'
import logout from './logout.js'
import login from './login.js'

import providersRouter from '../../auth/providers/index.js'

const router = express.Router()

router.get('/me', auth, me)
router.post('/logout', auth, logout)
// The credential exchange, addressed to one provider -- deliberately NOT
// behind the `auth` middleware, since it is the route that establishes the
// session in the first place. Mounted as a param route because provider ids
// contain a ':' ('external:clerk'), which a literal route pattern would parse
// as a param marker.
router.post('/login/:providerId', login)
router.use('/', providersRouter)

export default router
