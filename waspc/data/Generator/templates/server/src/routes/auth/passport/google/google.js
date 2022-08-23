import express from 'express'
import passport from 'passport'
import GoogleStrategy from 'passport-google-oauth20'

import waspServerConfig from '../../../../config.js'
import { contextWithUserEntity, authConfig, findOrCreateUserBySocialLogin } from '../../utils.js'
import { sign } from '../../../../core/auth.js'
import { configFn, getUserFields } from './googleConfig.js'

const config = ((config) => {
  if (!config?.clientId) {
    throw new Error("auth.google.configFn must return an object with a clientId property.")
  }

  if (!config?.clientSecret) {
    throw new Error("auth.google.configFn must return an object with a clientSecret property.")
  }

  if (!config?.scope) {
    throw new Error("auth.google.configFn must return an object with a scope property.")
  } else if (!Array.isArray(config.scope) || !config.scope.includes('profile')) {
    throw new Error("auth.google.configFn returned an object with an invalid scope property. It must be an array including 'profile'.")
  }

  return config
})(configFn())

passport.use('waspGoogleLoginStrategy', new GoogleStrategy({
  clientID: config.clientId,
  clientSecret: config.clientSecret,
  callbackURL: `${waspServerConfig.frontendUrl}/auth/login/google`,
  scope: config.scope,
  passReqToCallback: true
}, addGoogleProfileToRequest))

// This function is invoked after we successfully exchange the one-time-use OAuth code for a real Google API token.
// This token was used to get the Google profile information supplied as a parameter.
// We add the Google profile to the request for downstream use.
async function addGoogleProfileToRequest(req, _accessToken, _refreshToken, googleProfile, done) {
  req.wasp = { ...req.wasp, googleProfile }

  done(null, {})
}

const router = express.Router()

// Constructs a Google OAuth URL and redirects browser to start sign in flow.
router.get('/login', passport.authenticate('waspGoogleLoginStrategy', { session: false }))

// Validates the OAuth code from the frontend, via server-to-server communication
// with Google. If valid, provides frontend a response containing the JWT.
// NOTE: `addGoogleProfileToRequest` is invoked as part of the `passport.authenticate`
// call, before the final route handler callback. This is how we gain access to `req.wasp.googleProfile`. 
router.get('/validateCodeForLogin',
  passport.authenticate('waspGoogleLoginStrategy', {
    session: false,
    failureRedirect: waspServerConfig.frontendUrl + authConfig.failureRedirectPath
  }),
  async function (req, res) {
    const googleProfile = req?.wasp?.googleProfile

    if (!googleProfile) {
      throw new Error('Missing Google profile on request. This should not happen! Please contact Wasp.')
    } else if (!googleProfile.id) {
      throw new Error("Google profile was missing required id property. This should not happen! Please contact Wasp.")
    }

    const userFieldsPromise = getUserFields(contextWithUserEntity, { profile: googleProfile })
    const user = await findOrCreateUserBySocialLogin('google', googleProfile.id, userFieldsPromise)

    const token = await sign(user.id)
    res.json({ token })
  })

export default router
