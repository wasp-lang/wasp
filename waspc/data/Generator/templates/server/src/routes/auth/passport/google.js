{{={= =}=}}
import express from 'express'
import passport from 'passport'
import GoogleStrategy from 'passport-google-oauth20'
import { v4 as uuidv4 } from 'uuid'

import prisma from '../../../dbClient.js'
import waspServerConfig from '../../../config.js'
import { authConfig } from '../config.js'

// TODO: What if this name collides? For example, they import { config }
// and we imported config from '../../../config.js'?
{=& configJsFnImportStatement =}

export const GOOGLE_AUTH_METHOD = Symbol('Google')

const context = { entities: { {= userEntityUpper =}: prisma.{= userEntityLower =} } }

async function googleCallback(req, _accessToken, _refreshToken, profile, done) {
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

const callbackPath = '/oauth2/redirect'

passport.use(new GoogleStrategy({
  clientID: userConfig.clientId,
  clientSecret: userConfig.clientSecret,
  callbackURL: `/auth/external/google${callbackPath}`,
  scope: [ 'email', 'profile' ],
  passReqToCallback: true
}, googleCallback))

const router = express.Router()

router.get('/login', passport.authenticate('google', { session: false }))

router.get(callbackPath,
  passport.authenticate('google', {
    session: false,
    failureRedirect: waspServerConfig.frontendUrl + authConfig.failureRedirectPath
  }),
  async function (req, res) {
    const userId = req.wasp.userId

    if (!userId) {
      console.error('In Google OAuth success callback, but userId not in request. This should not happen!')
      return res.redirect(waspServerConfig.frontendUrl + authConfig.failureRedirectPath)
    }

    const otpToken = await prisma.otpToken.create({ data: { userId, token: uuidv4() }})
    res.redirect(`${waspServerConfig.frontendUrl}/tokenExchange?otpToken=${otpToken.token}`)
  })

export default router
