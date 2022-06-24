{{={= =}=}}
import GoogleStrategy from 'passport-google-oauth2'

import prisma from '../../dbClient.js'
import waspServerConfig from '../../config.js'
import { findOrCreateUserEntity } from '../auth.js'

// TODO: What if this name collides? For example, they import { config }
// and we imported config from '../../config.js'?
{=& configJsFnImportStatement =}

// TEMP: Just testing what a user would provide.
async function onSignInFn(_method, _context, args) {
  // TODO: Generate a secure, random password.
  const user = await findOrCreateUserEntity(args.profile.email, "password123!")
  return user
}

async function googleCallback(req, accessToken, refreshToken, profile, done) {
  console.log("In Google OAuth callback", accessToken, refreshToken, profile)

  const context = { entities: { {= userEntityUpper =}: prisma.{= userEntityLower =} } }
  // TODO: Make "google" a referenceable symbol.
  const user = await onSignInFn("google", context, { profile })

  req.session = { userId: user.id }

  return done(null, user)
}

export function setupGoogleAuth(app, passport) {
  // TODO: Verify we have what we need.
  const userConfig = {= configJsFnIdentifier =}()

  passport.use(new GoogleStrategy.Strategy({
    clientID: userConfig.clientID,
    clientSecret: userConfig.clientSecret,
    callbackURL: userConfig.callbackURL,
    scope: userConfig.scope || ['email'],
    passReqToCallback: true
  }, googleCallback))

  // TODO: ensure these routes do not require CORS/CSRF protection.
  // Redirect user to Google.
  app.get('/login/federated/google', passport.authenticate('google', { session: false }))

  // Handle Google callback.
  // TODO: Use onAuthFailedRedirectTo, etc.
  app.get('/oauth2/redirect/google',
    passport.authenticate('google', {
      session: false,
      failureRedirect: `${waspServerConfig.frontendUrl}/login`
    }),
    function (_req, res) {
      res.redirect(`${waspServerConfig.frontendUrl}/profile`)
    })
}
