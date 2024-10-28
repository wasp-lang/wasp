import merge from 'lodash.merge'

import { stripTrailingSlash } from "../universal/url.js";

const nodeEnv = process.env.NODE_ENV ?? 'development'

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
    port: process.env.PORT ? parseInt(process.env.PORT) : 3001,
    databaseUrl: process.env.DATABASE_URL,
    allowedCORSOrigins: [],
  },
  development: getDevelopmentConfig(),
  production: getProductionConfig(),
}

const resolvedConfig: Config = merge(config.all, config[nodeEnv])
// PUBLIC API
export default resolvedConfig

function getDevelopmentConfig(): EnvConfig {
  const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL ?? 'http://localhost:3000/');
  const serverUrl = stripTrailingSlash(process.env.WASP_SERVER_URL ?? 'http://localhost:3001');
  return {
    // @ts-ignore
    frontendUrl,
    // @ts-ignore
    serverUrl,
    allowedCORSOrigins: '*',
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
  }
}
