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
    {=# isAuthEnabled =}
    session: {
      name: process.env.SESSION_NAME || 'wasp',
      secret: undefined,
      cookie: {
        maxAge: parseInt(process.env.SESSION_COOKIE_MAX_AGE) || 7 * 24 * 60 * 60 * 1000, // ms
      }, 
    },
    {=/ isAuthEnabled =}
    frontendUrl: undefined,
  },
  development: {
    {=# isAuthEnabled =}
    session: {
      secret: process.env.SESSION_SECRET || 'sessionSecret',
    },
    {=/ isAuthEnabled =}
    frontendUrl: process.env.REACT_APP_URL || 'http://localhost:3000',
  },
  production: {
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
