{{={= =}=}}
import merge from 'lodash.merge'

import { env } from './env.js'
import { stripTrailingSlash } from '../universal/url.js'

// TODO:
//   - Use dotenv library to consume env vars from a file.
//   - Use convict library to define schema and validate env vars.
//  https://codingsans.com/blog/node-config-best-practices

type BaseConfig = {
  allowedCORSOrigins: string | string[];
}

type CommonConfig = BaseConfig & {
  env: string;
  isDevelopment: boolean;
  port: number;
  databaseUrl: string | undefined;
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: string | undefined;
  }
  {=/ isAuthEnabled =}
}

type EnvConfig = BaseConfig & {
  frontendUrl: string;
  serverUrl: string;
}

type Config = CommonConfig & EnvConfig

const config: {
  all: CommonConfig,
  development: EnvConfig,
  production: EnvConfig,
} = {
  all: {
    env: env.NODE_ENV,
    isDevelopment: env.NODE_ENV === 'development',
    port: env.PORT,
    databaseUrl: env.{= databaseUrlEnvVarName =},
    allowedCORSOrigins: [],
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: env.JWT_SECRET
    }
    {=/ isAuthEnabled =}
  },
  development: getDevelopmentConfig(),
  production: getProductionConfig(),
}

const resolvedConfig: Config = merge(config.all, config[env.NODE_ENV])
// PUBLIC API
export default resolvedConfig

function getDevelopmentConfig(): EnvConfig {
  const frontendUrl = stripTrailingSlash(env.WASP_WEB_CLIENT_URL);
  const serverUrl = stripTrailingSlash(env.WASP_SERVER_URL);
  return {
    frontendUrl,
    serverUrl,
    allowedCORSOrigins: '*',
  }
}

function getProductionConfig(): EnvConfig {
  const frontendUrl = stripTrailingSlash(env.WASP_WEB_CLIENT_URL);
  const serverUrl = stripTrailingSlash(env.WASP_SERVER_URL);
  return {
    frontendUrl,
    serverUrl,
    allowedCORSOrigins: [frontendUrl],
  }
}
