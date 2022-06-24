{{={= =}=}}
import GoogleStrategy from 'passport-google-oauth2'

import prisma from '../../dbClient.js'

{=& configJsFnImportStatement =}

// TEMP: Just testing what a user would provide.
async function onSignInFn(_method, _context, args) {
  const user = await findOrCreateUserEntity(args.profile.email)
  return user
}

async function findOrCreateUserEntity(email) {
  // TODO: Remove race condition.
  // TODO: Don't assume entity name is prisma.user.
  let user = await prisma.user.findUnique({ where: { email } })
  if (!user) {
    // TODO: Generate a secure, random password.
    user = await prisma.user.create({ data: { email, password: "password123!" } })
  }
  return user
}

async function googleCallback(req, accessToken, refreshToken, profile, done) {
  console.log("In Google OAuth callback", accessToken, refreshToken, profile)

  const context = { entities: { User: prisma.user } }
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

  // Redirect user to Google.
  app.get('/login/federated/google', passport.authenticate('google', { session: false }))

  // Handle Google callback.
  // TODO: Use onAuthFailedRedirectTo, etc.
  app.get('/oauth2/redirect/google',
    passport.authenticate('google', {
      session: false,
      failureRedirect: 'http://localhost:3000/login'
    }),
    function (_req, res) {
      res.redirect('http://localhost:3000/profile')
    })
}
