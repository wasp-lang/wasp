import type { PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { type AppName, waspBuild } from "../waspCli.js";
import { buildAndRunClientApp } from "./client.js";
import { buildAndRunServerApp } from "./server.js";

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
  await waspBuild({
    waspCliCmd,
    pathToApp,
  });

  const { dbEnvVars } = await setupDb({
    appName,
    dbType,
    pathToApp,
  });

  await buildAndRunServerApp({
    appName,
    pathToApp,
    extraEnv: dbEnvVars,
  });

  await buildAndRunClientApp({
    pathToApp,
  });
}
