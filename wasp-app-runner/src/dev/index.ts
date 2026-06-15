import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { waitUntilAppStops } from "../run.js";
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
  signal: AbortSignal;
}): Promise<void> {
  await using db = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
    signal,
  });

  await waspMigrateDb({
    waspCliCmd,
    pathToApp,
    extraEnv: db.dbEnvVars,
    signal,
  });

  await using app = waspStart({
    waspCliCmd,
    pathToApp,
    extraEnv: db.dbEnvVars,
    signal,
  });

  await waitUntilAppStops({ app, services: [db], signal });
}
