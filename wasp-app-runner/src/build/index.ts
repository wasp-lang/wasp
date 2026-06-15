import * as path from "node:path";
import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { doesFileExist } from "../files.js";
import { waitUntilAppStops } from "../run.js";
import { startLocalSmtpServer } from "../smtp.js";
import { EnvVars } from "../types.js";
import { type AppName, waspBuild, waspBuildStart } from "../waspCli.js";

export async function startAppInBuildMode({
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
  await waspBuild({
    waspCliCmd,
    pathToApp,
    signal,
  });

  await using db = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
    signal,
  });

  await using smtp = await startLocalSmtpServer({
    appName,
    pathToApp,
    signal,
  });

  const serverEnvVars: EnvVars = {
    JWT_SECRET: "some-jwt-secret",
    ...db.dbEnvVars,
  };

  const serverEnvFile = path.resolve(pathToApp, ".env.server");
  const clientEnvFile = path.resolve(pathToApp, ".env.client");

  await using app = waspBuildStart({
    waspCliCmd,
    pathToApp,
    serverEnvVars,
    serverEnvFile: doesFileExist(serverEnvFile) ? serverEnvFile : undefined,
    clientEnvFile: doesFileExist(clientEnvFile) ? clientEnvFile : undefined,
    signal,
  });

  await waitUntilAppStops({ app, services: [db, smtp], signal });
}
