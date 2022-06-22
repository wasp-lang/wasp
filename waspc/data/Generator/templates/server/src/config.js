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
    // This option is sometimes needed when running behind proxies/load balancers.
    // For example, this is required for secure cookies to work on Heroku.
    // Ref: https://expressjs.com/en/guide/behind-proxies.html
    // For now, we only handle the boolean case.
    trustProxies: undefined,
    {=# isAuthEnabled =}
    session: {
      cookie: {
        name: process.env.SESSION_COOKIE_NAME || 'wasp_session',
        secret: undefined,
        maxAgeMs: parseInt(process.env.SESSION_COOKIE_MAX_AGE) || 7 * 24 * 60 * 60 * 1000, // 1 week in ms
      },
    },
    csrf: {
      cookie: {
        name: process.env.CSRF_COOKIE_NAME || 'wasp_csrf',
      },
    },
    {=/ isAuthEnabled =}
    frontendUrl: undefined,
  },
  development: {
    trustProxies: toBooleanOrDefault(process.env.TRUST_PROXIES, false),
    {=# isAuthEnabled =}
    session: {
      cookie: {
        secret: process.env.SESSION_COOKIE_SECRET || 'sessionSecret',
      },
    },
    {=/ isAuthEnabled =}
    frontendUrl: process.env.REACT_APP_URL || 'http://localhost:3000',
  },
  production: {
    trustProxies: toBooleanOrDefault(process.env.TRUST_PROXIES, true),
    {=# isAuthEnabled =}
    session: {
      cookie: {
        secret: process.env.SESSION_COOKIE_SECRET,
      },
    },
    {=/ isAuthEnabled =}
    frontendUrl: process.env.REACT_APP_URL,
  }
}

const resolvedConfig = _.merge(config.all, config[env])
export default resolvedConfig

function toBooleanOrDefault(str, defaultValue) {
  if (!str) {
    return defaultValue
  }

  switch(str.toLowerCase()) {
    case "t":
    case "true":
      return true
    case "f":
    case "false":
      return false
    default:
      return defaultValue
  }
}

export function checkCookieSecretLength(secret) {
  if (!secret || secret.length < 32) {
    throw new Error("SESSION_COOKIE_SECRET must be at least 32 characters long in production")
  }
}
