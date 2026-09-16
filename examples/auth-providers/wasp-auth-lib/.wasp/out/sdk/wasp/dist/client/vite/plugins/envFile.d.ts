import { type Plugin, type UserConfig } from 'vite';
export declare function envFile(): Plugin;
export declare function loadEnvVars({ rootDir, envPrefix, loadDotEnvFile, }: {
    rootDir: string;
    envPrefix: NonNullable<UserConfig['envPrefix']>;
    loadDotEnvFile: boolean;
}): Promise<Record<string, string>>;
//# sourceMappingURL=envFile.d.ts.map