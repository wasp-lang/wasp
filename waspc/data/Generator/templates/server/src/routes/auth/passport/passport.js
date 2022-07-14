{{={= =}=}}
import express from 'express'

{=# isGoogleAuthEnabled =}
import googleAuth from './google/google.js'
{=/ isGoogleAuthEnabled =}

const router = express.Router()

{=# isGoogleAuthEnabled =}
router.use('/google', googleAuth)
{=/ isGoogleAuthEnabled =}

export default router
