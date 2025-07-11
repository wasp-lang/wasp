{{={= =}=}}
import { env } from './env.js'
import { stripTrailingSlash } from '../universal/url.js'

type NodeEnv = typeof env.NODE_ENV

type Config = {
  env: NodeEnv;
  isDevelopment: boolean;
  port: number;
  databaseUrl: string;
  frontendUrl: string;
  serverUrl: string;
  allowedCORSOrigins: string | string[];
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: string;
  }
  {=/ isAuthEnabled =}
}

const frontendUrl = stripTrailingSlash(env["{= clientUrlEnvVarName =}"])
const serverUrl = stripTrailingSlash(env["{= serverUrlEnvVarName =}"])

const allowedCORSOriginsPerEnv: Record<NodeEnv, string | string[]> = {
  development: '*',
  production: [frontendUrl]
}
const allowedCORSOrigins = allowedCORSOriginsPerEnv[env.NODE_ENV]

const config: Config = {
  frontendUrl,
  serverUrl,
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
