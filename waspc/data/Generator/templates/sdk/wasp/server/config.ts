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
  cookieEnabled: boolean;
  {=# isCookieAuthEnabled =}
  allowedOrigins: string[];
  apiPrefix: string;
  cookieName: string;
  cookieExpires: boolean;
  cookieSecure: boolean;
  cookieSameSite: 'lax' | 'strict' | 'none';
  cookiePath: string;
  sessionExpiresIn: number;
  {=/ isCookieAuthEnabled =}
  allowedCORSOrigins: string | string[];
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: string;
  }
  {=/ isAuthEnabled =}
}

const frontendUrl = stripTrailingSlash(env.WASP_WEB_CLIENT_URL)
const serverUrl = stripTrailingSlash(env.WASP_SERVER_URL)
{=# isCookieAuthEnabled =}
const allowedOrigins = env.ALLOWED_ORIGINS.split(',').map((origin) => origin.trim())
const apiPrefix = stripTrailingSlash(env.WASP_API_PREFIX)
const cookieName = env.COOKIE_NAME
const cookieExpires = env.COOKIE_EXPIRES
const cookieSecure = env.COOKIE_SECURE
const cookieSameSite = env.COOKIE_SAME_SITE
const cookiePath = env.COOKIE_PATH
const sessionExpiresIn = Number(env.SESSION_EXPIRES_IN_SECONDS)
{=/ isCookieAuthEnabled =}

const allowedCORSOriginsPerEnv: Record<NodeEnv, string | string[]> = {
  development: [frontendUrl],
  production: [frontendUrl]
}
const allowedCORSOrigins = allowedCORSOriginsPerEnv[env.NODE_ENV]

const config: Config = {
  frontendUrl,
  serverUrl,
  allowedCORSOrigins,
  {=# isCookieAuthEnabled =}
  cookieEnabled: true,
  allowedOrigins,
  apiPrefix,
  cookieName,
  cookieExpires,
  cookieSecure,
  cookieSameSite,
  cookiePath,
  sessionExpiresIn,
  {=/ isCookieAuthEnabled =}
  {=^ isCookieAuthEnabled =}
  cookieEnabled: false,
  {=/ isCookieAuthEnabled =}
  env: env.NODE_ENV,
  isDevelopment: env.NODE_ENV === 'development',
  port: env.PORT,
  databaseUrl: env.{= databaseUrlEnvVarName =},
  {=# isAuthEnabled =}
  auth: {
    jwtSecret: env.JWT_SECRET
  }
  {=/ isAuthEnabled =}
}

// PUBLIC API
export default config
