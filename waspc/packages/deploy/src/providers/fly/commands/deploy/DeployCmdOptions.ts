import { CommonCmdOptions, LocalBuildOptions } from "../../CommonCmdOptions.js";

export interface DeployCmdOptions extends CommonCmdOptions, LocalBuildOptions {
  skipClient?: boolean;
  skipServer?: boolean;
  customServerUrl?: string;
}
