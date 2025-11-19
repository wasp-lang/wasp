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
  auth: {
    jwtSecret: string;
  }
}

const frontendUrl = stripTrailingSlash(env['WASP_WEB_CLIENT_URL'])
const serverUrl = stripTrailingSlash(env['WASP_SERVER_URL'])

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
  databaseUrl: env.DATABASE_URL,
  auth: {
    jwtSecret: env["JWT_SECRET"]
  }
}

// PUBLIC API
export default config
