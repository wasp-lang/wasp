import { env } from './env.js';
import { stripTrailingSlash } from '../universal/url.js';
const frontendUrl = stripTrailingSlash(env.WASP_WEB_CLIENT_URL);
const serverUrl = stripTrailingSlash(env.WASP_SERVER_URL);
const allowedCORSOriginsPerEnv = {
    development: '*',
    production: [frontendUrl]
};
const allowedCORSOrigins = allowedCORSOriginsPerEnv[env.NODE_ENV];
const config = {
    frontendUrl,
    serverUrl,
    allowedCORSOrigins,
    env: env.NODE_ENV,
    isDevelopment: env.NODE_ENV === 'development',
    port: env.PORT,
    databaseUrl: env.DATABASE_URL,
    auth: {
        jwtSecret: env.JWT_SECRET
    }
};
// PUBLIC API
export default config;
//# sourceMappingURL=config.js.map