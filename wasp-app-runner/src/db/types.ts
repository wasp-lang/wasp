import type { ManagedService } from "../run.js";
import type { EnvVars } from "../types.js";

export interface SetupDbResult extends ManagedService {
  dbEnvVars: EnvVars;
}
