import passport from 'passport'
import GoogleStrategy from 'passport-google-oauth2'

import prisma from './dbClient.js'

async function defaultGoogleCallback(req, accessToken, refreshToken, profile, done) {
  console.log("In Google OAuth callback", accessToken, refreshToken, profile)

  // TODO: Remove race condition.
  let user = await prisma.user.findUnique({ where: { email: profile.email } });
  if (!user) {
    // TODO: Generate a secure, random password.
    user = await prisma.user.create({ data: { email: profile.email, password: "password123!" } })
  }

  req.session = { user_id: user.id }

  return done(null, user)
}

// TODO: Make use of optional user callback.
// TODO: Update all the magic URLs.
export function usePassport(app) {
  app.use(passport.initialize())

  passport.use(new GoogleStrategy.Strategy({
    clientID: process.env['GOOGLE_CLIENT_ID'],
    clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
    callbackURL: 'http://localhost:3001/oauth2/redirect/google',
    scope: ['email'],
    passReqToCallback: true
  }, defaultGoogleCallback))

  // Redirect user to Google
  app.get('/login/federated/google', passport.authenticate('google', { session: false }))

  // Handle Google callback
  app.get('/oauth2/redirect/google',
    passport.authenticate('google', {
      session: false,
      failureRedirect: 'http://localhost:3000/login'
    }),
    function (_req, res) {
      res.redirect('http://localhost:3000/profile')
    })
}
