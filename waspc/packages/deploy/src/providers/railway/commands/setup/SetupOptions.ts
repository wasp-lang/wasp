import { RailwayProjectId } from "../../brandedTypes.js";
import { CommonOptions, SecretsOptions } from "../../CommonOptions.js";

export interface SetupOptions extends CommonOptions, SecretsOptions {
  existingProjectId: RailwayProjectId | null;
}
