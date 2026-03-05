import {
  CommonCmdOptions,
  CustomServerUrlOption,
  LocalBuildOptions,
} from "../../CommonCmdOptions.js";

export interface DeployCmdOptions
  extends CommonCmdOptions,
    LocalBuildOptions,
    CustomServerUrlOption {
  skipClient?: boolean;
  skipServer?: boolean;
}
