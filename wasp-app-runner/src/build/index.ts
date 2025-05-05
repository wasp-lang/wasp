import type { PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { type AppName, buildApp } from "../waspCli.js";
import { buildAndStartClientApp } from "./client.js";
import { buildAndRunServerAppContainer } from "./server.js";

export async function startAppInBuildMode({
  waspCliCmd,
  pathToApp,
  appName,
  dbType,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  appName: AppName;
  dbType: DbType;
}) {
  await buildApp({
    waspCliCmd,
    pathToApp,
  });

  const { dbEnvVars } = await setupDb({
    appName,
    dbType,
    pathToApp,
  });

  await buildAndRunServerAppContainer({
    appName,
    pathToApp,
    extraEnv: dbEnvVars,
  });

  await buildAndStartClientApp({
    pathToApp,
  });
}
