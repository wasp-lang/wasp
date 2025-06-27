import { CommonOptions } from "../../CommonOptions.js";

export interface DeployOptions extends CommonOptions {
  skipClient?: boolean;
  skipServer?: boolean;
}
