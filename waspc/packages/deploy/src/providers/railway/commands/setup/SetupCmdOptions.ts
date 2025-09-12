import { RailwayProjectId } from "../../brandedTypes.js";
import {
  CommonCmdOptions,
  SecretsOptions,
} from "../../DeploymentInstructions.js";

export interface SetupCmdOptions extends CommonCmdOptions, SecretsOptions {
  existingProjectId: RailwayProjectId | null;
  workspace: string | null;
}
