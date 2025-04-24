import { env } from './env.js';
type NodeEnv = typeof env.NODE_ENV;
type Config = {
    env: NodeEnv;
    isDevelopment: boolean;
    port: number;
    databaseUrl: string;
    frontendUrl: string;
    serverUrl: string;
    allowedCORSOrigins: string | string[];
    auth: {
        jwtSecret: string;
    };
};
declare const config: Config;
export default config;
//# sourceMappingURL=config.d.ts.map