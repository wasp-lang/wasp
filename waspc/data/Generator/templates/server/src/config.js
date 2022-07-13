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
    frontendUrl: undefined,
    // This option is sometimes needed when running behind proxies/load balancers.
    // For example, this is required for relative paths with Passport to work on Heroku.
    // Ref: https://expressjs.com/en/guide/behind-proxies.html
    // For now, we only handle the boolean case.
    trustProxies: undefined,
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: undefined
    }
    {=/ isAuthEnabled =}
  },
  development: {
    frontendUrl: process.env.WASP_WEB_CLIENT_URL || 'http://localhost:3000',
    trustProxies: parseBooleanOrDefault(process.env.TRUST_PROXIES, false),
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: 'DEVJWTSECRET'
    }
    {=/ isAuthEnabled =}
  },
  production: {
    frontendUrl: process.env.WASP_WEB_CLIENT_URL,
    trustProxies: parseBooleanOrDefault(process.env.TRUST_PROXIES, true),
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: process.env.JWT_SECRET
    }
    {=/ isAuthEnabled =}
  }
}

function parseBooleanOrDefault(str, defaultValue) {
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

const resolvedConfig = _.merge(config.all, config[env])
export default resolvedConfig
