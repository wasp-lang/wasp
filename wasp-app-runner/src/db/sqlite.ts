import type { SetupDbResult } from "./types.js";

export async function setupSqlite(): Promise<SetupDbResult> {
  // SQLite needs no external service: no env vars, nothing to tear down,
  // and nothing that can die unexpectedly (`exited: null`).
  return {
    name: "sqlite",
    exited: null,
    dbEnvVars: {},
    async [Symbol.asyncDispose]() {},
  };
}
