{{={= =}=}}
import merge from 'lodash.merge'

import { stripTrailingSlash } from "../universal/url.js";

const nodeEnv = process.env.NODE_ENV ?? 'development'

// TODO:
//   - Use dotenv library to consume env vars from a file.
//   - Use convict library to define schema and validate env vars.
//  https://codingsans.com/blog/node-config-best-practices

type BaseConfig = {
  allowedCORSOrigins: string | string[];
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: string | undefined;
  }
  {=/ isAuthEnabled =}
}

type CommonConfig = BaseConfig & {
  env: string;
  isDevelopment: boolean;
  port: number;
  databaseUrl: string | undefined;
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
    env: nodeEnv,
    isDevelopment: nodeEnv === 'development',
    port: process.env.PORT ? parseInt(process.env.PORT) : {= defaultServerPort =},
    databaseUrl: process.env.{= databaseUrlEnvVarName =},
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

const resolvedConfig: Config = merge(config.all, config[nodeEnv])
// PUBLIC API
export default resolvedConfig

function getDevelopmentConfig(): EnvConfig {
  const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL ?? '{= defaultClientUrl =}');
  const serverUrl = stripTrailingSlash(process.env.WASP_SERVER_URL ?? '{= defaultServerUrl =}');
  return {
    // @ts-ignore
    frontendUrl,
    // @ts-ignore
    serverUrl,
    allowedCORSOrigins: '*',
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: 'DEVJWTSECRET'
    }
    {=/ isAuthEnabled =}
  }
}

function getProductionConfig(): EnvConfig {
  const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL);
  const serverUrl = stripTrailingSlash(process.env.WASP_SERVER_URL);
  return {
    // @ts-ignore
    frontendUrl,
    // @ts-ignore
    serverUrl,
    // @ts-ignore
    allowedCORSOrigins: [frontendUrl],
    {=# isAuthEnabled =}
    auth: {
      jwtSecret: process.env.JWT_SECRET
    }
    {=/ isAuthEnabled =}
  }
}
