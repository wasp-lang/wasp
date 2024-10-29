import merge from 'lodash.merge';
import { env } from './env.js';
import { stripTrailingSlash } from '../universal/url.js';
const config = {
    all: {
        env: env.NODE_ENV,
        isDevelopment: env.NODE_ENV === 'development',
        port: env.PORT,
        databaseUrl: env.DATABASE_URL,
        allowedCORSOrigins: [],
    },
    development: getDevelopmentConfig(),
    production: getProductionConfig(),
};
const resolvedConfig = merge(config.all, config[env.NODE_ENV]);
// PUBLIC API
export default resolvedConfig;
function getDevelopmentConfig() {
    const frontendUrl = stripTrailingSlash(env.WASP_WEB_CLIENT_URL);
    const serverUrl = stripTrailingSlash(env.WASP_SERVER_URL);
    return {
        frontendUrl,
        serverUrl,
        allowedCORSOrigins: '*',
    };
}
function getProductionConfig() {
    const frontendUrl = stripTrailingSlash(env.WASP_WEB_CLIENT_URL);
    const serverUrl = stripTrailingSlash(env.WASP_SERVER_URL);
    return {
        frontendUrl,
        serverUrl,
        allowedCORSOrigins: [frontendUrl],
    };
}
//# sourceMappingURL=config.js.map