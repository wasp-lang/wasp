type BaseConfig = {
    allowedCORSOrigins: string | string[];
};
type CommonConfig = BaseConfig & {
    env: string;
    isDevelopment: boolean;
    port: number;
    databaseUrl: string | undefined;
};
type EnvConfig = BaseConfig & {
    frontendUrl: string;
    serverUrl: string;
};
type Config = CommonConfig & EnvConfig;
declare const resolvedConfig: Config;
export default resolvedConfig;
