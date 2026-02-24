import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { type AppName, waspMigrateDb, waspStart } from "../waspCli.js";

export async function startAppInDevMode({
  waspCliCmd,
  pathToApp,
  appName,
  dbType,
  dbImage,
  signal,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  appName: AppName;
  dbType: DbType;
  dbImage: DockerImageName;
  signal?: AbortSignal;
}): Promise<void> {
  const { dbEnvVars } = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
    signal,
  });

  await waspMigrateDb({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
    signal,
  });

  await waspStart({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
    signal,
  });
}
