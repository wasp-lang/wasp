import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.ts";
import { DbType, setupDb } from "../db/index.ts";
import { type AppName, waspMigrateDb, waspStart } from "../waspCli.ts";

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
  const { dbEnvVars } = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
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
