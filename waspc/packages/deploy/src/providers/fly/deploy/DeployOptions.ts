import { CommonOptions } from '../CommonOptions.js';

export interface DeployOptions extends CommonOptions {
	skipBuild?: boolean;
	skipClient?: boolean;
	skipServer?: boolean;
}
