{{={= =}=}}
import GoogleStrategy from 'passport-google-oauth2'

import prisma from '../../dbClient.js'
import waspServerConfig from '../../config.js'

// TODO: What if this name collides? For example, they import { config }
// and we imported config from '../../config.js'?
{=& configJsFnImportStatement =}

function googleCallback(onSignInFn) {
  return async (req, accessToken, refreshToken, profile, done) => {
    console.log("In Google OAuth callback", accessToken, refreshToken, profile)

    const context = { entities: { {= userEntityUpper =}: prisma.{= userEntityLower =} } }
    // TODO: Make "google" a referenceable symbol.
    const user = await onSignInFn("google", context, { profile })

    if (!user?.id) {
      throw new Error("auth.onSignInFn must return an object with an id property")
    }

    req.session = { userId: user.id }

    return done(null, user)
  }
}

export function setupGoogleAuth(app, passport, authConfig) {
  // TODO: Verify we have what we need.
  const userConfig = {= configJsFnIdentifier =}()

  passport.use(new GoogleStrategy.Strategy({
    clientID: userConfig.clientID,
    clientSecret: userConfig.clientSecret,
    callbackURL: userConfig.callbackURL,
    scope: userConfig.scope || ['email'],
    passReqToCallback: true
  }, googleCallback(authConfig.onSignInFn)))

  // Redirect user to Google.
  // TODO: Clean up duplication with button.
  app.get('/login/federated/google', passport.authenticate('google', { session: false }))

  // Handle Google callback.
  // TODO: Should we constrain what path users can use here?
  const callbackURL = new URL(userConfig.callbackURL)
  app.get(callbackURL.pathname,
    passport.authenticate('google', {
      session: false,
      failureRedirect: waspServerConfig.frontendUrl + authConfig.failureRedirectPath
    }),
    function (_req, res) {
      res.redirect(waspServerConfig.frontendUrl + authConfig.successRedirectPath)
    })
}
