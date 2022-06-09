{{={= =}=}}
import _ from 'lodash'

const env = process.env.NODE_ENV || 'development'

// TODO:
//   - Use dotenv library to consume env vars from a file.
//   - Use convict library to define schema and validate env vars.
//  https://codingsans.com/blog/node-config-best-practices

const config = {
  all: {
    env,
    port: parseInt(process.env.PORT) || 3001,
    databaseUrl: process.env.DATABASE_URL,
    trustProxyCount: undefined,
    {=# isAuthEnabled =}
    session: {
      name: process.env.SESSION_NAME || 'wasp_session',
      secret: undefined,
      cookie: {
        maxAge: parseInt(process.env.SESSION_COOKIE_MAX_AGE) || 7 * 24 * 60 * 60 * 1000, // ms
      }, 
    },
    {=/ isAuthEnabled =}
    frontendUrl: undefined,
  },
  development: {
    trustProxyCount: parseInt(process.env.TRUST_PROXY_COUNT) || 0,
    {=# isAuthEnabled =}
    session: {
      secret: process.env.SESSION_SECRET || 'sessionSecret',
    },
    {=/ isAuthEnabled =}
    frontendUrl: process.env.REACT_APP_URL || 'http://localhost:3000',
  },
  production: {
    trustProxyCount: parseInt(process.env.TRUST_PROXY_COUNT) || 1,
    {=# isAuthEnabled =}
    session: {
      secret: process.env.SESSION_SECRET,
    },
    {=/ isAuthEnabled =}
    frontendUrl: process.env.REACT_APP_URL,
  }
}

const resolvedConfig = _.merge(config.all, config[env])
export default resolvedConfig
