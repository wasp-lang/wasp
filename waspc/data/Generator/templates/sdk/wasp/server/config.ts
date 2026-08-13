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
  allowedCORSOrigins: (string | RegExp)[];
  {=# isWaspAuthUsed =}
  auth: {
    jwtSecret: string;
  }
  {=/ isWaspAuthUsed =}
}

const frontendUrl = stripTrailingSlash(env['{= clientUrlEnvVarName =}'])
const serverUrl = stripTrailingSlash(env['{= serverUrlEnvVarName =}'])

const allowedCORSOriginsPerEnv: Record<NodeEnv, Config['allowedCORSOrigins']> = {
  development: [/.*/],
  production: [getOrigin(frontendUrl)]
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
  {=# isWaspAuthUsed =}
  auth: {
    jwtSecret: env["{= jwtSecretEnvVarName =}"]
  }
  {=/ isWaspAuthUsed =}
}

// PUBLIC API
export default config
