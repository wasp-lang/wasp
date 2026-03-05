import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { type AppName, waspMigrateDb, waspStart } from "../waspCli.js";

export async function startAppInDevMode({
  waspCliCmd,
  pathToApp,
  appName,
  dbType,
  dbImage,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  appName: AppName;
  dbType: DbType;
  dbImage: DockerImageName;
}): Promise<void> {
  await using db = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
  });

  const { dbEnvVars } = await db.waitUntilReady();

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
