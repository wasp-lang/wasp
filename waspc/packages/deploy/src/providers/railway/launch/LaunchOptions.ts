import { DeployOptions } from "../deploy/DeployOptions.js";
import { SetupOptions } from "../setup/SetupOptions.js";

export interface LaunchOptions extends SetupOptions, DeployOptions {}
