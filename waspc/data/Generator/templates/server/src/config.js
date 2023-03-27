{{={= =}=}}
import merge from 'lodash.merge'

import { stripTrailingSlash } from "./universal/url.js";

const env = process.env.NODE_ENV || 'development'

// TODO:
//   - Use dotenv library to consume env vars from a file.
//   - Use convict library to define schema and validate env vars.
//  https://codingsans.com/blog/node-config-best-practices

const config = {
  all: {
    env,
    isDevelopment: env === 'development',
    port: parseInt(process.env.PORT) || 3001,
    databaseUrl: process.env.{= databaseUrlEnvVarName =},
    frontendUrl: undefined,
    allowedCORSOrigins: [],
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: undefined
    }
    {=/ isAuthEnabled =}
  },
  development: getDevelopmentConfig(),
  production: getProductionConfig(),
}

const resolvedConfig = merge(config.all, config[env])
export default resolvedConfig

function getDevelopmentConfig() {
  const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL) || 'http://localhost:3000';
  return {
    frontendUrl,
    allowedCORSOrigins: '*',
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: 'DEVJWTSECRET'
    }
    {=/ isAuthEnabled =}
  }
}

function getProductionConfig() {
  const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL);
  return {
    frontendUrl,
    allowedCORSOrigins: [frontendUrl],
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: process.env.JWT_SECRET
    }
    {=/ isAuthEnabled =}
  }
}
