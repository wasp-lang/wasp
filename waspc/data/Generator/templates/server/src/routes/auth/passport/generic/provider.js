import express from 'express'
import passport from 'passport'

import waspServerConfig from '../../../../config.js'
import { contextWithUserEntity, authConfig, findOrCreateUserByExternalAuthAssociation } from '../../utils.js'
import { sign } from '../../../../core/auth.js'

// This function is invoked after we successfully exchange the one-time-use OAuth code for a real provider API token.
// This token was used to get the provider profile information supplied as a parameter.
// We add the provider profile to the request for downstream use.
async function addProviderProfileToRequest(req, _accessToken, _refreshToken, providerProfile, done) {
  req.wasp = { ...req.wasp, providerProfile }

  done(null, {})
}

export function initRouter(providerName, ProviderStrategy, config, getUserFieldsFn) {
  const router = express.Router()
  const passportStrategyName = `wasp${providerName}LoginStrategy`

  const requiredConfig = {
    callbackURL: `${waspServerConfig.frontendUrl}/auth/login/${providerName}`,
    passReqToCallback: true
  }
  passport.use(passportStrategyName,
    new ProviderStrategy({ ...config, ...requiredConfig }, addProviderProfileToRequest))

  // Constructs a provider OAuth URL and redirects browser to start sign in flow.
  router.get('/login', passport.authenticate(passportStrategyName, { session: false }))

  // Validates the OAuth code from the frontend, via server-to-server communication
  // with provider. If valid, provides frontend a response containing the JWT.
  // NOTE: `addProviderProfileToRequest` is invoked as part of the `passport.authenticate`
  // call, before the final route handler callback. This is how we gain access to `req.wasp.providerProfile`. 
  router.get('/validateCodeForLogin',
    passport.authenticate(passportStrategyName, {
      session: false,
      failureRedirect: waspServerConfig.frontendUrl + authConfig.failureRedirectPath
    }),
    async function (req, res) {
      const providerProfile = req?.wasp?.providerProfile

      if (!providerProfile) {
        throw new Error(`Missing ${providerName} provider profile on request. This should not happen! Please contact Wasp.`)
      } else if (!providerProfile.id) {
        throw new Error(`${providerName} provider profile was missing required id property. This should not happen! Please contact Wasp.`)
      }

      // Wrap call to getUserFieldsFn so we can invoke only if needed.
      const getUserFields = () => getUserFieldsFn(contextWithUserEntity, { profile: providerProfile })
      const user = await findOrCreateUserByExternalAuthAssociation(providerName, providerProfile.id, getUserFields)

      const token = await sign(user.id)
      res.json({ token })
    })

  return router
}
