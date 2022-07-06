{{={= =}=}}
import express from 'express'
import cookieParser from 'cookie-parser'
import logger from 'morgan'
import cors from 'cors'
import helmet from 'helmet'

import HttpError from './core/HttpError.js'
import indexRouter from './routes/index.js'

{=# isPassportRequired =}
import { usePassport } from './core/auth/passport/passport.js'
{=/ isPassportRequired =}

// TODO: Consider extracting most of this logic into createApp(routes, path) function so that
//   it can be used in unit tests to test each route individually.

const app = express()

app.use(helmet())
app.use(cors()) // TODO: Consider configuring CORS to be more restrictive, right now it allows all CORS requests.
app.use(logger('dev'))
app.use(express.json())
app.use(express.urlencoded({ extended: false }))
app.use(cookieParser())

import prisma from './dbClient.js'
import { sign } from './core/auth.js'

{=# isPassportRequired =}
usePassport(app)
{=/ isPassportRequired =}

// TODO: Figure out why this cannot go into usePassport
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
