{{={= =}=}}
import _ from 'lodash'
{=! NOTE: validators are copied from shared templates =}
import { isValidAbsoluteURL } from './validators/validators.js';

const env = process.env.NODE_ENV || 'development'

// TODO:
//   - Use dotenv library to consume env vars from a file.
//   - Use convict library to define schema and validate env vars.
//  https://codingsans.com/blog/node-config-best-practices

if (process.env.WASP_WEB_CLIENT_URL && !isValidAbsoluteURL(process.env.WASP_WEB_CLIENT_URL)) {
  throw 'Environment variable WASP_WEB_CLIENT_URL is not a valid absolute URL';
}

const frontendUrl = process.env.WASP_WEB_CLIENT_URL || 'http://localhost:3000'

const config = {
  all: {
    env,
    port: parseInt(process.env.PORT) || 3001,
    databaseUrl: process.env.DATABASE_URL,
    frontendUrl: undefined,
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: undefined
    }
    {=/ isAuthEnabled =}
  },
  development: {
    frontendUrl,
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: 'DEVJWTSECRET'
    }
    {=/ isAuthEnabled =}
  },
  production: {
    frontendUrl: process.env.WASP_WEB_CLIENT_URL,
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: process.env.JWT_SECRET
    }
    {=/ isAuthEnabled =}
  }
}

const resolvedConfig = _.merge(config.all, config[env])
export default resolvedConfig
