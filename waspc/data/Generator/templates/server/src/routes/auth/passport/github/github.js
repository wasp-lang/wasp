import express from 'express'
import passport from 'passport'
import GithubStrategy from 'passport-github2'

import waspServerConfig from '../../../../config.js'
import { contextWithUserEntity, authConfig, findOrCreateUserByExternalAuthAssociation } from '../../utils.js'
import { sign } from '../../../../core/auth.js'
import { configFn, getUserFieldsFn } from './githubConfig.js'

// Validates the provided config function returns all required data.
const config = ((config) => {
  if (!config?.clientId) {
    throw new Error("auth.github.configFn must return an object with a clientId property.")
  }

  if (!config?.clientSecret) {
    throw new Error("auth.github.configFn must return an object with a clientSecret property.")
  }

  if (!config?.scope || !Array.isArray(config.scope)) {
    throw new Error("auth.github.configFn must return an object with a scope property.")
  }

  return config
})(await configFn())

passport.use('waspGithubLoginStrategy', new GithubStrategy({
  clientID: config.clientId,
  clientSecret: config.clientSecret,
  callbackURL: `${waspServerConfig.frontendUrl}/auth/login/github`,
  passReqToCallback: true
}, addGithubProfileToRequest))

// This function is invoked after we successfully exchange the one-time-use OAuth code for a real Github API token.
// This token was used to get the Github profile information supplied as a parameter.
// We add the Github profile to the request for downstream use.
async function addGithubProfileToRequest(req, _accessToken, _refreshToken, githubProfile, done) {
  console.log(githubProfile)
  req.wasp = { ...req.wasp, githubProfile }

  done(null, {})
}

const router = express.Router()

// Constructs a Github OAuth URL and redirects browser to start sign in flow.
router.get('/login', passport.authenticate('waspGithubLoginStrategy', { session: false }))

// Validates the OAuth code from the frontend, via server-to-server communication
// with Github. If valid, provides frontend a response containing the JWT.
// NOTE: `addGithubProfileToRequest` is invoked as part of the `passport.authenticate`
// call, before the final route handler callback. This is how we gain access to `req.wasp.githubProfile`. 
router.get('/validateCodeForLogin',
  passport.authenticate('waspGithubLoginStrategy', {
    session: false,
    failureRedirect: waspServerConfig.frontendUrl + authConfig.failureRedirectPath
  }),
  async function (req, res) {
    const githubProfile = req?.wasp?.githubProfile

    if (!githubProfile) {
      throw new Error('Missing Github profile on request. This should not happen! Please contact Wasp.')
    } else if (!githubProfile.id) {
      throw new Error("Github profile was missing required id property. This should not happen! Please contact Wasp.")
    }

    // Wrap call to getUserFieldsFn so we can invoke only if needed.
    const getUserFields = () => getUserFieldsFn(contextWithUserEntity, { profile: githubProfile })
    const user = await findOrCreateUserByExternalAuthAssociation('github', githubProfile.id, getUserFields)

    const token = await sign(user.id)
    res.json({ token })
  })

export default router
