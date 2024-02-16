type BaseConfig = {
    allowedCORSOrigins: string | string[];
    auth: {
        jwtSecret: string | undefined;
    };
};
type CommonConfig = BaseConfig & {
    env: string;
    isDevelopment: boolean;
    port: number;
    databaseUrl: string | undefined;
};
type EnvConfig = BaseConfig & {
    frontendUrl: string;
};
type Config = CommonConfig & EnvConfig;
declare const resolvedConfig: Config;
export default resolvedConfig;
