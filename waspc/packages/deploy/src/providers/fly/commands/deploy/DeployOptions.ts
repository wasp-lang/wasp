import { CommonOptions, LocalBuildOptions } from "../../CommonOptions.js";

export interface DeployOptions extends CommonOptions, LocalBuildOptions {
  skipClient?: boolean;
  skipServer?: boolean;
}
