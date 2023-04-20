import { CommonOptions } from '../../shared/CommonOptions';

export interface RwDeployOptions extends CommonOptions {
    skipClient?: boolean;
    skipServer?: boolean;
    skipBuild?: boolean;
}
