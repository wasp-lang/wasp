import merge from 'lodash.merge';
import { stripTrailingSlash } from "wasp/universal/url";
const env = process.env.NODE_ENV || 'development';
const config = {
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
};
const resolvedConfig = merge(config.all, config[env]);
// PUBLIC API
export default resolvedConfig;
function getDevelopmentConfig() {
    const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL || 'http://localhost:3000/');
    return {
        frontendUrl,
        allowedCORSOrigins: '*',
        auth: {
            jwtSecret: 'DEVJWTSECRET'
        }
    };
}
function getProductionConfig() {
    const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL);
    return {
        frontendUrl,
        allowedCORSOrigins: [frontendUrl],
        auth: {
            jwtSecret: process.env.JWT_SECRET
        }
    };
}
//# sourceMappingURL=config.js.map