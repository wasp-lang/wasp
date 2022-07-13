import express from 'express'
import cookieParser from 'cookie-parser'
import logger from 'morgan'
import cors from 'cors'
import helmet from 'helmet'

import HttpError from './core/HttpError.js'
import indexRouter from './routes/index.js'
import config from './config.js'

// TODO: Consider extracting most of this logic into createApp(routes, path) function so that
//   it can be used in unit tests to test each route individually.

const app = express()

app.use(helmet())
app.use(cors({
  // TODO: Consider allowing users to provide a function to further configure CORS setup.
  origin: config.frontendUrl,
}))
app.use(logger('dev'))
app.use(express.json())
app.use(express.urlencoded({ extended: false }))
app.use(cookieParser())

if (config.trustProxies) {
  app.enable('trust proxy')
  console.log("Trusting proxies")
}

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
