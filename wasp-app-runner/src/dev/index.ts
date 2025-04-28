import { DbType, setupDb } from "../db/index.js";
import { migrateDb, startApp } from "../waspCli.js";

export async function startAppInDevMode({
  waspCliCmd,
  pathToApp,
  appName,
  dbType,
}: {
  waspCliCmd: string;
  pathToApp: string;
  appName: string;
  dbType: DbType;
}): Promise<void> {
  const { dbEnvVars } = await setupDb({
    appName,
    dbType,
    pathToApp,
  });

  await migrateDb({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
  });

  await startApp({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
  });
}
