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
    maxAge: config.session.cookie.maxAge,
  },
  store: new PrismaSessionStore(prisma, {
    checkPeriod: 2 * 60 * 1000,  //ms
    dbRecordIdIsSessionId: true,
    dbRecordIdFunction: undefined
  })
}

export function useSession(app) {
  if (app.get('env') === 'production') {
    sess.cookie.secure = true
  }
  app.use(session(sess))
}
