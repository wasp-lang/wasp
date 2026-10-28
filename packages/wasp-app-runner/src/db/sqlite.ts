import type { SetupDbResult } from "./types.js";

export const setupSqlite = async (): Promise<SetupDbResult> => {
  // No need to do anything special for SQLite, just return
  // an empty object for the env vars.
  return {
    dbEnvVars: {},
  };
};
