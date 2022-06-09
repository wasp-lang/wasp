import cookieSession from 'cookie-session'
import csrf from 'csurf'

import config from './config.js'

const sessionConfig = {
  name: config.session.name,
  secret: config.session.secret,
  httpOnly: true,
  signed: true,
  maxAge: config.session.cookie.maxAge,
}

const csrfConfig = {
  cookie: {
    key: 'wasp_csrf',
    httpOnly: true,
  },
}

export function useSession(app) {
  if (config.env === 'production') {
    sessionConfig.secure = true
    sessionConfig.sameSite = 'none'
    csrfConfig.cookie.secure = true
    csrfConfig.cookie.sameSite = 'none'
  }

  app.use(cookieSession(sessionConfig))
  app.use(csrf(csrfConfig))
}
