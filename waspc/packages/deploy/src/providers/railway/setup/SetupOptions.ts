import { Branded } from "../../../common/branded.js";
import { CommonOptions, SecretsOptions } from "../CommonOptions.js";

export type RailwayProjectId = Branded<string, "RailwayProjectId">;

export interface SetupOptions extends CommonOptions, SecretsOptions {
  existingProjectId: RailwayProjectId | null;
}
