import type { SetupDbFn } from "./types.js";

export const setupSqlite: SetupDbFn = async (_options) => {
  // No need to do anything special for SQLite, just return
  // an empty object for the env vars.
  return {
    dbEnvVars: {},
  };
};
