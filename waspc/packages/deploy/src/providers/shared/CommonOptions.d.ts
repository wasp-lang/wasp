export interface CommonOptions {
    waspExe: string;
    waspProjectDir: string;
    flyTomlDir?: string;
}

export interface DbOptions {
    vmSize: string;
    initialClusterSize: string;
    volumeSize: string;
}

export interface LocalBuildOptions {
    buildLocally: boolean;
}
