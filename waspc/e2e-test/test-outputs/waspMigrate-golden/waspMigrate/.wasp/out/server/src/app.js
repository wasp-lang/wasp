import express from 'express'

import { HttpError } from 'wasp/server'
import indexRouter from './routes/index.js'

// TODO: Consider extracting most of this logic into createApp(routes, path) function so that
//   it can be used in unit tests to test each route individually.

const app = express()

// NOTE: Middleware are installed on a per-router or per-route basis.

app.use('/', indexRouter)

// Custom error handler.
app.use((err, _req, res, next) => {
  // As by expressjs documentation, when the headers have already
  // been sent to the client, we must delegate to the default error handler.
  if (res.headersSent) { return next(err) }

  if (err instanceof HttpError) {
    return res.status(err.statusCode).json({ message: err.message, data: err.data })
  }

  // This forwards the error to the default express error handler.
  // As described by expressjs documentation, the default error handler sets response status
  // to err.status or err.statusCode if it is 4xx or 5xx, and if not, sets it to 500.
  // It won't add any more info to it if server is running in production, which is exactly what we want,
  // we want to share as little info as possible when error happens in production, for security reasons,
  // so they will get only status code if set, or 500 if not, no extra info.
  // In development it will also share the error stack though, which is useful.
  // If the user wants to put more information about the error into the response, they should use HttpError.
  return next(err)
})

export default app
