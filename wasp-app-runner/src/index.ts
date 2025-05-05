import { log } from "./logging.js";
import { checkDependencies } from "./dependencies.js";
import { DbType } from "./db/index.js";
import { getAppInfo } from "./waspCli.js";
import { startAppInDevMode } from "./dev/index.js";
import { startAppInBuildMode } from "./build/index.js";
import { type Mode, parseArgs, PathToApp, WaspCliCmd } from "./args.js";

export async function main(): Promise<void> {
  const { mode, waspCliCmd, pathToApp } = parseArgs();

  try {
    await runWaspApp({
      mode,
      waspCliCmd,
      pathToApp,
    });
  } catch (error: unknown) {
    if (error instanceof Error) {
      log("main", "error", `Fatal error: ${error.message}`);
    } else {
      log("main", "error", `Fatal error: ${error}`);
    }
    process.exit(1);
  }
}

async function runWaspApp({
  mode,
  waspCliCmd,
  pathToApp,
}: {
  mode: Mode;
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<void> {
  await checkDependencies();

  const { appName, dbType } = await getAppInfo({
    waspCliCmd,
    pathToApp,
  });

  if (dbType === DbType.Sqlite && mode === "build") {
    log(
      "setup",
      "error",
      `SQLite is not supported in build mode. Please use a different database type (e.g., Postgres) or run in dev mode.`
    );
    process.exit(1);
  }

  log(
    "setup",
    "info",
    `Starting "${appName}" app (mode: ${mode}) using "${waspCliCmd}" command`
  );

  if (mode === "dev") {
    await startAppInDevMode({
      waspCliCmd,
      pathToApp,
      appName,
      dbType,
    });
  } else {
    await startAppInBuildMode({
      waspCliCmd,
      pathToApp,
      appName,
      dbType,
    });
  }
}
