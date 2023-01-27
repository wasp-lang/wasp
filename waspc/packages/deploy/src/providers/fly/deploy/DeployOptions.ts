import { GlobalOptions } from '../GlobalOptions.js';

export interface DeployOptions extends GlobalOptions {
	skipBuild?: boolean;
}
