import { CommonOptions, LocalBuildOptions } from '../CommonOptions.js';

export interface DeployOptions extends CommonOptions, LocalBuildOptions {
	skipBuild?: boolean;
	skipClient?: boolean;
	skipServer?: boolean;
}
