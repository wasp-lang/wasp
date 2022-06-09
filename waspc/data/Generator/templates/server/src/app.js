import express from 'express'
import cookieParser from 'cookie-parser'
import logger from 'morgan'
import cors from 'cors'
import helmet from 'helmet'
import session from 'express-session'

import HttpError from './core/HttpError.js'
import indexRouter from './routes/index.js'

// TODO: Consider extracting most of this logic into createApp(routes, path) function so that
//   it can be used in unit tests to test each route individually.

const app = express()

app.use(helmet())
// TESTING
app.use(cors({
  origin: 'http://localhost:3000',
  methods: ['POST', 'PUT', 'GET', 'OPTIONS', 'HEAD'],
  credentials: true
}));
app.use(logger('dev'))
app.use(express.json())
app.use(express.urlencoded({ extended: false }))
app.use(cookieParser())
app.use(session({
  name: 'wasp',
  secret: 'keyboard cat',
  resave: false,
  saveUninitialized: true,
  cookie: {
    httpOnly: true,
    secure: false,
    maxAge: 604800000 // 1 week
  }
}))

// TESTING
app.use(function(req, res, next) {
  console.log("SessionID: " + req.sessionID);
  next();
});

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
