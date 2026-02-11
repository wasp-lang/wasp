{{={= =}=}}
import { waspEnv } from './waspEnv.js';
import { stripTrailingSlash, getOrigin } from '../universal/url.js'

type NodeEnv = typeof waspEnv.NODE_ENV

type Config = {
  env: NodeEnv;
  isDevelopment: boolean;
  port: number;
  databaseUrl: string;
  frontendUrl: string;
  serverUrl: string;
  allowedCORSOrigins: (string | RegExp)[];
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: string;
  }
  {=/ isAuthEnabled =}
}

const frontendUrl = stripTrailingSlash(waspEnv['{= clientUrlEnvVarName =}'])
const serverUrl = stripTrailingSlash(waspEnv['{= serverUrlEnvVarName =}'])

const allowedCORSOriginsPerEnv: Record<NodeEnv, Config['allowedCORSOrigins']> = {
  development: [/.*/],
  production: [getOrigin(frontendUrl)]
}
const allowedCORSOrigins = allowedCORSOriginsPerEnv[waspEnv.NODE_ENV]

const config: Config = {
  frontendUrl,
  serverUrl,
  allowedCORSOrigins,
  env: waspEnv.NODE_ENV,
  isDevelopment: waspEnv.NODE_ENV === 'development',
  port: waspEnv.PORT,
  databaseUrl: waspEnv.{= databaseUrlEnvVarName =},
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: waspEnv["{= jwtSecretEnvVarName =}"]
  }
  {=/ isAuthEnabled =}
}

// PUBLIC API
export default config
