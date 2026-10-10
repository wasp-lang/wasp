import express from 'express'

import { toHttpError } from 'wasp/server/errors'
import indexRouter from './routes/index.js'

// TODO: Consider extracting most of this logic into createApp(routes, path) function so that
//   it can be used in unit tests to test each route individually.

const app = express()

// NOTE: Middleware are installed on a per-router or per-route basis.

app.use('/', indexRouter)

// Custom error handler.
app.use(/** @type {import('express').ErrorRequestHandler} */ ((err, _req, res, next) => {
  // As by expressjs documentation, when the headers have already
  // been sent to the client, we must delegate to the default error handler.
  if (res.headersSent) { return next(err) }

  const httpError = toHttpError(err, 'request')
  return res.status(httpError.statusCode).json({ message: httpError.message, data: httpError.data })
}))

export default app
