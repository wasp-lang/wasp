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
  // TODO: update the docs to mention this
  const backendUrl = stripTrailingSlash(process.env.WASP_SERVER_URL) || 'http://localhost:3001';
  return {
    frontendUrl,
    backendUrl,
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
  // TODO: make this mandatory when deploying (use it when deploying with Fly)
  const backendUrl = stripTrailingSlash(process.env.WASP_SERVER_URL);
  return {
    frontendUrl,
    backendUrl,
    allowedCORSOrigins: [frontendUrl],
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: process.env.JWT_SECRET
    }
    {=/ isAuthEnabled =}
  }
}
