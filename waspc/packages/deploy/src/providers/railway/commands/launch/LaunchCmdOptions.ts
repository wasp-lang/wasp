import { DeployCmdOptions } from "../deploy/DeployCmdOptions.js";
import { SetupCmdOptions } from "../setup/SetupCmdOptions.js";

export interface LaunchCmdOptions extends SetupCmdOptions, DeployCmdOptions {}
