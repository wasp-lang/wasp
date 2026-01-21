import * as path from "path";
import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { doesFileExist } from "../files.js";
import { startLocalSmtpServer } from "../smtp.js";
import { EnvVars } from "../types.js";
import { type AppName, waspBuild, waspBuildStart } from "../waspCli.js";

export async function startAppInBuildMode({
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

  const serverEnvVars: EnvVars = {
    JWT_SECRET: "some-jwt-secret",
    ...dbEnvVars,
  };

  const serverEnvFile = path.resolve(pathToApp, ".env.server");
  const clientEnvFile = path.resolve(pathToApp, ".env.client");

  await waspBuildStart({
    waspCliCmd,
    pathToApp,
    serverEnvVars,
    serverEnvFile: doesFileExist(serverEnvFile) ? serverEnvFile : undefined,
    clientEnvFile: doesFileExist(clientEnvFile) ? clientEnvFile : undefined,
  });
}
