import type { RunAppWithDbFn } from "./index.js";

export const runAppWithSqlite: RunAppWithDbFn = async (_options, runApp) => {
  // No need to do anything special for SQLite, just run the app.
  await runApp({
    extraEnv: {},
  });
};
