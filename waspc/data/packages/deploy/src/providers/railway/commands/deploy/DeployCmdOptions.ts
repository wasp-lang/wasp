import { RailwayProjectId } from "../../brandedTypes.js";
import {
  CommonCmdOptions,
  CustomServerUrlOption,
} from "../../DeploymentInstructions.js";

export interface DeployCmdOptions
  extends CommonCmdOptions,
    CustomServerUrlOption {
  skipClient?: boolean;
  skipServer?: boolean;
  existingProjectId?: RailwayProjectId;
}
