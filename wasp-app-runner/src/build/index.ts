import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { startLocalSmtpServer } from "../smtp.js";
import type { VersionSettings } from "../versions.js";
import { type AppName, waspBuild } from "../waspCli.js";
import { buildAndRunClientApp } from "./client.js";
import { buildAndRunServerApp } from "./server.js";

// Based on https://github.com/wasp-lang/wasp/issues/1883#issuecomment-2766265289
export async function startAppInBuildMode({
  waspCliCmd,
  pathToApp,
  appName,
  versionSettings,
  dbType,
  dbImage,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  appName: AppName;
  versionSettings: VersionSettings;
  dbType: DbType;
  dbImage: DockerImageName;
}) {
  await waspBuild({
    waspCliCmd,
    pathToApp,
  });

  const { dbEnvVars } = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
  });

  await startLocalSmtpServer();

  // Client needs to be running before the server
  // because `playwright` tests start executing as soon
  // as the server is up.
  await buildAndRunClientApp({
    pathToApp,
    versionSettings,
  });

  await buildAndRunServerApp({
    appName,
    pathToApp,
    extraEnv: dbEnvVars,
    versionSettings,
  });
}
