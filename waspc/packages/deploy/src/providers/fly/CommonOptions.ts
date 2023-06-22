export interface CommonOptions {
	waspExe: string;
	waspProjectDir: string;
	flyTomlDir?: string;
	org?: string;
}

export interface DbOptions {
	vmSize: string;
	initialClusterSize: string;
	volumeSize: string;
}

export interface LocalBuildOptions {
	buildLocally: boolean;
}

export interface SecretsOptions {
	serverSecret: string[];
	clientSecret: string[];
}
