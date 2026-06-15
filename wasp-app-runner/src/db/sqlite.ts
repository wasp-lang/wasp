import type { SetupDbResult } from "./types.js";

export async function setupSqlite(): Promise<SetupDbResult> {
  return {
    name: "sqlite",
    exited: null,
    dbEnvVars: {},
    async [Symbol.asyncDispose]() {},
  };
}
