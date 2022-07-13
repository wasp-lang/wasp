{{={= =}=}}
import express from 'express'
import passport from 'passport'
import GoogleStrategy from 'passport-google-oauth20'
import { v4 as uuidv4 } from 'uuid'

import prisma from '../../../dbClient.js'
import waspServerConfig from '../../../config.js'
import { authConfig } from '../config.js'
import { sign } from '../../../core/auth.js'

// TODO: What if this name collides? For example, they import { config }
// and we imported config from '../../../config.js'?
{=& configJsFnImportStatement =}

export const GOOGLE_AUTH_METHOD = Symbol('Google')

const context = { entities: { {= userEntityUpper =}: prisma.{= userEntityLower =} } }

async function googleSuccessCallback(req, _accessToken, _refreshToken, profile, done) {
  try {
    const user = await authConfig.onSignInFn(GOOGLE_AUTH_METHOD, context, { profile })

    if (!user?.id) {
      return done(new Error('auth.onSignInFn must return a user object with an id property'))
    }

    req.wasp = { ...req.wasp, userId: user.id }

    done(null, user)
  } catch (err) {
    return done(err)
  }
}

const userConfig = validateConfig({= configJsFnIdentifier =}())

function validateConfig(config) {
  if (!config?.clientId) {
    throw new Error("auth.google.configFn must return an object with clientId property")
  }

  if (!config?.clientSecret) {
    throw new Error("auth.google.configFn must return an object with clientSecret property")
  }

  return config
}

passport.use(new GoogleStrategy({
  clientID: userConfig.clientId,
  clientSecret: userConfig.clientSecret,
  callbackURL: `${waspServerConfig.frontendUrl}/auth/redirect/google`,
  scope: [ 'email', 'profile' ],
  passReqToCallback: true
}, googleSuccessCallback))

const router = express.Router()

// Constructs a Google OAuth URL and redirects browser.
router.get('/login', passport.authenticate('google', { session: false }))

// Validates the OAuth code from the frontend, via server-to-server communication
// with Google. If valid, provides them with a JWT.
router.get('/validateCode',
  passport.authenticate('google', {
    session: false,
    failureRedirect: waspServerConfig.frontendUrl + authConfig.failureRedirectPath
  }),
  async function (req, res) {
    const userId = req.wasp.userId

    if (!userId) {
      console.error('In Google OAuth success callback, but userId not in request. This should not happen!')
      return res.status(500).send()
    }

    const token = await sign(userId)
    res.json({ token })
  })

export default router
