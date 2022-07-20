import express from 'express'
import passport from 'passport'
import GoogleStrategy from 'passport-google-oauth20'

import waspServerConfig from '../../../../config.js'
import { contextWithUserEntity, authConfig } from '../../utils.js'
import { sign } from '../../../../core/auth.js'
import { configFn, onSignInFn } from './googleConfig.js'

const config = validateConfig(configFn())

passport.use('waspGoogleStrategy', new GoogleStrategy({
  clientID: config.clientId,
  clientSecret: config.clientSecret,
  callbackURL: `${waspServerConfig.frontendUrl}/auth/redirect/google`,
  scope: config.scope,
  passReqToCallback: true
}, oauthCodeValidationSucceeded))

// This function is invoked after we successfully exchange the one-time-use OAuth code for a real Google API token.
// This token was used to get the Google profile information supplied as a parameter.
async function oauthCodeValidationSucceeded(req, _accessToken, _refreshToken, profile, done) {
  try {
    const user = await onSignInFn(contextWithUserEntity, { profile })

    if (!user?.id) {
      return done(new Error('auth.onSignInFn must return a user object with an id property'))
    }

    // Pass along the userId so we can create the JWT in the OAuth code validation route handler.
    req.wasp = { ...req.wasp, userId: user.id }

    done(null, user)
  } catch (err) {
    return done(err)
  }
}

function validateConfig(config) {
  if (!config?.clientId) {
    throw new Error("auth.google.configFn must return an object with a clientId property.")
  }

  if (!config?.clientSecret) {
    throw new Error("auth.google.configFn must return an object with a clientSecret property.")
  }

  if (!config?.scope) {
    throw new Error("auth.google.configFn must return an object with a scope property.")
  } else if (!Array.isArray(config.scope) || !config.scope.includes('email') || !config.scope.includes('profile')) {
    throw new Error("auth.google.configFn returned an object with an invalid scope property. It must be an array including 'email' and 'profile'.")
  }

  return config
}

const router = express.Router()

// Constructs a Google OAuth URL and redirects browser to start sign in flow.
router.get('/login', passport.authenticate('waspGoogleStrategy', { session: false }))

// Validates the OAuth code from the frontend, via server-to-server communication
// with Google. If valid, provides frontend a response containing the JWT.
// NOTE: `oauthCodeValidationSucceeded` is invoked as part of the `passport.authenticate`
// call, before the final route handler callback. This is how we gain access to `req.wasp`. 
router.get('/validateCode',
  passport.authenticate('waspGoogleStrategy', {
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
