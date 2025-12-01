import { CommonCmdOptions } from "../../CommonCmdOptions.js";

export interface DeployCmdOptions extends CommonCmdOptions {
  skipClient?: boolean;
  skipServer?: boolean;
  customServerUrl?: string;
}
