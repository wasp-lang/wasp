import type { RunAppWithDbFn } from "./types.js";

export const runAppWithSqlite: RunAppWithDbFn = async (_options, runApp) => {
  // No need to do anything special for SQLite, just run the app.
  return runApp({
    extraEnv: {},
  });
};
