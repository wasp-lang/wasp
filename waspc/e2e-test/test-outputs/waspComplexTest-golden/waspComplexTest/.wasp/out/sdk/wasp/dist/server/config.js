var _a;
import merge from 'lodash.merge';
import { stripTrailingSlash } from "../universal/url.js";
const nodeEnv = (_a = process.env.NODE_ENV) !== null && _a !== void 0 ? _a : 'development';
const config = {
    all: {
        env: nodeEnv,
        isDevelopment: nodeEnv === 'development',
        port: process.env.PORT ? parseInt(process.env.PORT) : 3001,
        databaseUrl: process.env.DATABASE_URL,
        allowedCORSOrigins: [],
        auth: {
            jwtSecret: undefined
        }
    },
    development: getDevelopmentConfig(),
    production: getProductionConfig(),
};
const resolvedConfig = merge(config.all, config[nodeEnv]);
// PUBLIC API
export default resolvedConfig;
function getDevelopmentConfig() {
    var _a, _b;
    const frontendUrl = stripTrailingSlash((_a = process.env.WASP_WEB_CLIENT_URL) !== null && _a !== void 0 ? _a : 'http://localhost:3000/');
    const serverUrl = stripTrailingSlash((_b = process.env.WASP_SERVER_URL) !== null && _b !== void 0 ? _b : 'http://localhost:3001');
    return {
        // @ts-ignore
        frontendUrl,
        // @ts-ignore
        serverUrl,
        allowedCORSOrigins: '*',
        auth: {
            jwtSecret: 'DEVJWTSECRET'
        }
    };
}
function getProductionConfig() {
    const frontendUrl = stripTrailingSlash(process.env.WASP_WEB_CLIENT_URL);
    const serverUrl = stripTrailingSlash(process.env.WASP_SERVER_URL);
    return {
        // @ts-ignore
        frontendUrl,
        // @ts-ignore
        serverUrl,
        // @ts-ignore
        allowedCORSOrigins: [frontendUrl],
        auth: {
            jwtSecret: process.env.JWT_SECRET
        }
    };
}
//# sourceMappingURL=config.js.map