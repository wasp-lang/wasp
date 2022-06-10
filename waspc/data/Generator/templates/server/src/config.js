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
    session: {
      name: process.env.SESSION_NAME || 'wasp',
      secret: undefined,
      trustProxyCount: parseInt(process.env.SESSION_TRUST_PROXY_COUNT) || 0,
    },
    frontendUrl: undefined,
  },
  development: {
    session: {
      secret: process.env.SESSION_SECRET || 'sessionSecret',
    },
    frontendUrl: process.env.REACT_APP_URL || 'http://localhost:3000',
  },
  production: {
    session: {
      secret: process.env.SESSION_SECRET,
    },
    frontendUrl: process.env.REACT_APP_URL,
  }
}

const resolvedConfig = _.merge(config.all, config[env])
export default resolvedConfig
