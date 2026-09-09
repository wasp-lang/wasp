{{={= =}=}}
import { env } from './env.js'
import { stripTrailingSlash, getOrigin } from '../universal/url.js'

type NodeEnv = typeof env.NODE_ENV

// PUBLIC API
export type Config = {
  env: NodeEnv;
  isDevelopment: boolean;
  port: number;
  databaseUrl: string;
  frontendUrl: string;
  serverUrl: string;
  /**
   * The client's base dir (`app.client.baseDir`) as a path prefix: `""` when the client lives
   * at the root, otherwise without a trailing slash (e.g. `"/my-app"`).
   */
  clientBaseDir: string;
  allowedCORSOrigins: (string | RegExp)[];
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: string;
  }
  {=/ isAuthEnabled =}
}

const clientBaseDir: string = '{=& clientBaseDir =}'

const serverUrl = stripTrailingSlash(env['{= serverUrlEnvVarName =}'])
{=# isSingleDeployment =}
// In single deployment mode the server serves the client, so the client URL defaults to the server URL.
const frontendUrl = getOrigin(env['{= clientUrlEnvVarName =}'] ?? env['{= serverUrlEnvVarName =}']) + clientBaseDir
{=/ isSingleDeployment =}
{=^ isSingleDeployment =}
const frontendUrl = stripTrailingSlash(env['{= clientUrlEnvVarName =}'])
{=/ isSingleDeployment =}

const allowedCORSOriginsPerEnv: Record<NodeEnv, Config['allowedCORSOrigins']> = {
  development: [/.*/],
  production: [getOrigin(frontendUrl)]
}
const allowedCORSOrigins = allowedCORSOriginsPerEnv[env.NODE_ENV]

const config: Config = {
  frontendUrl,
  serverUrl,
  clientBaseDir,
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
