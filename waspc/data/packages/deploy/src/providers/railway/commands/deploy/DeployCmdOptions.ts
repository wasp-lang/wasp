import { RailwayProjectId } from "../../brandedTypes.js";
import { CommonCmdOptions } from "../../DeploymentInstructions.js";

export interface DeployCmdOptions extends CommonCmdOptions {
  skipClient?: boolean;
  skipServer?: boolean;
  existingProjectId?: RailwayProjectId;
}
