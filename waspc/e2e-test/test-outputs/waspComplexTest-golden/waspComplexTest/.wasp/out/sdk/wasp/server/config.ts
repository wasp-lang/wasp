import merge from 'lodash.merge'

import { stripTrailingSlash } from "wasp/universal/url";

const env = process.env.NODE_ENV || 'development'

// TODO:
//   - Use dotenv library to consume env vars from a file.
//   - Use convict library to define schema and validate env vars.
//  https://codingsans.com/blog/node-config-best-practices

type BaseConfig = {
  allowedCORSOrigins: string | string[];
  auth: {
    jwtSecret: string | undefined;
  }
}

type CommonConfig = BaseConfig & {
  env: string;
  isDevelopment: boolean;
  port: number;
  databaseUrl: string | undefined;
}

type EnvConfig = BaseConfig & {
  frontendUrl: string;
}

type Config = CommonConfig & EnvConfig

const config: {
  all: CommonConfig,
  development: EnvConfig,
  production: EnvConfig,
} = {
  all: {
    env,
    isDevelopment: env === 'development',
    port: parseInt(process.env.PORT) || 3001,
    databaseUrl: process.env.DATABASE_URL,
    allowedCORSOrigins: [],
    auth: {
      jwtSecret: undefined
    }
  },
  development: getDevelopmentConfig(),
  production: getProductionConfig(),
}

const resolvedConfig: Config = merge(config.all, config[env])
// PUBLIC API
export default resolvedConfig

function getDevelopmentConfig(): EnvConfig {
  const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL || 'http://localhost:3000/');
  return {
    frontendUrl,
    allowedCORSOrigins: '*',
    auth: {
      jwtSecret: 'DEVJWTSECRET'
    }
  }
}

function getProductionConfig(): EnvConfig {
  const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL);
  return {
    frontendUrl,
    allowedCORSOrigins: [frontendUrl],
    auth: {
      jwtSecret: process.env.JWT_SECRET
    }
  }
}
