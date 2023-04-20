import { CommonOptions, LocalBuildOptions } from '../../shared/CommonOptions';

export interface FlyDeployOptions extends CommonOptions, LocalBuildOptions {
	skipBuild?: boolean;
	skipClient?: boolean;
	skipServer?: boolean;
}
