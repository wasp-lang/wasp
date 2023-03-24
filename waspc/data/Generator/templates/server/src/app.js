import express from 'express'

import HttpError from './core/HttpError.js'
import indexRouter from './routes/index.js'

// TODO: Consider extracting most of this logic into createApp(routes, path) function so that
//   it can be used in unit tests to test each route individually.

const app = express()

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
