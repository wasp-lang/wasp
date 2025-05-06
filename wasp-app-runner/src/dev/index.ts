import type { PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { type AppName, waspMigrateDb, waspStart } from "../waspCli.js";

export async function startAppInDevMode({
  waspCliCmd,
  pathToApp,
  appName,
  dbType,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  appName: AppName;
  dbType: DbType;
}): Promise<void> {
  const { dbEnvVars } = await setupDb({
    appName,
    dbType,
    pathToApp,
  });

  await waspMigrateDb({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
  });

  await waspStart({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
  });
}
