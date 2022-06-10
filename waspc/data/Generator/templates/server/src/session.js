import session from 'express-session'
import { PrismaSessionStore } from '@quixo3/prisma-session-store'

import config from './config.js'
import prisma from './dbClient.js'

const sess = {
  name: config.session.name,
  secret: config.session.secret,
  // NOTE: The two options below are kinda finiky with PrismaSessionStore.
  resave: false,
  saveUninitialized: true,
  cookie: {
    httpOnly: true,
    // TODO: Use sameSite ?
    maxAge: 7 * 24 * 60 * 60 * 1000 // ms
  },
  store: new PrismaSessionStore(prisma, {
    checkPeriod: 2 * 60 * 1000,  //ms
    dbRecordIdIsSessionId: true,
    dbRecordIdFunction: undefined
  })
}

export function initSession(app) {
  if (app.get('env') === 'production') {
    sess.cookie.secure = true
  }

  app.use(session(sess))

  if (config.session.trustProxyCount > 0) {
    app.set('trust proxy', config.session.trustProxyCount)
  }
}
