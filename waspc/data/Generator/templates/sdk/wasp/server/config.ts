{{={= =}=}}
import { env } from './env.js'
import { stripTrailingSlash, getOrigin } from '../universal/url.js'

type NodeEnv = typeof env.NODE_ENV

type Config = {
  env: NodeEnv;
  isDevelopment: boolean;
  port: number;
  databaseUrl: string;
  frontendUrl: string;
  serverUrl: string;
  serverBasePath: string;
  allowedCORSOrigins: (string | RegExp)[];
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: string;
  }
  {=/ isAuthEnabled =}
}

const frontendUrl = stripTrailingSlash(env['{= clientUrlEnvVarName =}'])
const serverUrl = stripTrailingSlash(env['{= serverUrlEnvVarName =}'])
// "" when it is the root, otherwise the path without a trailing slash (e.g. "/api").
const serverBasePath: string = '{=& serverBasePathPrefix =}'

const allowedCORSOriginsPerEnv: Record<NodeEnv, Config['allowedCORSOrigins']> = {
  development: [/.*/],
  production: [getOrigin(frontendUrl)]
}
const allowedCORSOrigins = allowedCORSOriginsPerEnv[env.NODE_ENV]

const config: Config = {
  frontendUrl,
  serverUrl,
  serverBasePath,
  allowedCORSOrigins,
  env: env.NODE_ENV,
  isDevelopment: env.NODE_ENV === 'development',
  port: env.PORT,
  databaseUrl: env.{= databaseUrlEnvVarName =},
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: env["{= jwtSecretEnvVarName =}"]
  }
  {=/ isAuthEnabled =}
}

// PUBLIC API
export default config
