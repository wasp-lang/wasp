import express from 'express'
import cookieParser from 'cookie-parser'
import logger from 'morgan'
import cors from 'cors'
import helmet from 'helmet'

import HttpError from './core/HttpError.js'
import indexRouter from './routes/index.js'

// TODO: Consider extracting most of this logic into createApp(routes, path) function so that
//   it can be used in unit tests to test each route individually.

const app = express()

app.use(helmet())
app.use(cors()) // TODO: Consider configuring CORS to be more restrictive, right now it allows all CORS requests.
app.use(logger('dev'))
app.use(express.json())
app.use(express.urlencoded({ extended: false }))
app.use(cookieParser())

import passport from 'passport'
import GoogleStrategy from 'passport-google-oauth2'
import { v4 as uuidv4 } from 'uuid'
import prisma from './dbClient.js'
import { sign } from './core/auth.js'

console.log('setting up Google')

passport.use(new GoogleStrategy.Strategy({
  clientID: process.env['GOOGLE_CLIENT_ID'],
  clientSecret: process.env['GOOGLE_CLIENT_SECRET'],
  callbackURL: 'http://localhost:3001/oauth2/redirect/google',
  scope: ['email'],
  passReqToCallback: true
}, async function (request, _accessToken, _refreshToken, profile, done) {
  console.log("In Google OAuth callback")

  let user = await prisma.user.findUnique({ where: { email: profile.email } })
  if (!user) {
    user = await prisma.user.create({ data: { email: profile.email, password: "password123!" } })
  }

  request.wasp = { ...request.wasp, userId: user.id }

  return done(null, user)
}))

// Redirect user to Google
app.get('/login/federated/google', passport.authenticate('google', { session: false }))

// Handle Google callback
app.get('/oauth2/redirect/google',
  passport.authenticate('google', {
    session: false,
    failureRedirect: 'http://localhost:3000/login',
  }),
  async function(req, res) {
    console.log("In Passport success callback")
    const userId = req.wasp.userId

    if (req.wasp.userId) {
      const otpToken = await prisma.otpToken.create({ data: { userId, token: uuidv4() }})
      res.redirect('http://localhost:3000/login?otpToken=' + otpToken.token)
    }
    else {
      // NOTE: Should not happen if auth was successful.
      console.error('In passport success callback, but user not in request.')
      res.redirect('http://localhost:3000/login')
    }
})

// This is a route that takes a 1-time use, time limited token
// to lookup what user just successfully logged in above.
app.post('/otpTokenExchange', async (req, res) => {
  const args = req.body || {}
  const now = new Date()
  const minuteInMilliseconds = 60 * 1000;
  const otpToken = await prisma.otpToken.findFirst({
    where: {
      token: args.otpToken,
      claimed: false,
      createdAt: {
        gte: new Date(now - minuteInMilliseconds),
        lte: now,
      },
    }
  })
  if (otpToken) {
    await prisma.otpToken.update({ where: { id: otpToken.id }, data: { claimed: true } })
    const token = await sign(otpToken.userId)
    return res.json({ token })
  }
  return res.status(401).send()
})

app.use('/', indexRouter)

// Custom error handler.
app.use((err, req, res, next) => {
  // As by expressjs documentation, when the headers have already
  // been sent to the client, we must delegate to the default error handler.
  if (res.headersSent) { return next(err) }

  if (err instanceof HttpError) {
    return res.status(err.statusCode).json({ message: err.message, data: err.data })
  }

  return next(err)
})

export default app
