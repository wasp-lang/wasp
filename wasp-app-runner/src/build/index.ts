import * as path from "node:path";
import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.ts";
import { DbType, setupDb } from "../db/index.ts";
import { doesFileExist } from "../files.ts";
import { startLocalSmtpServer } from "../smtp.ts";
import type { EnvVars } from "../types.ts";
import { type AppName, waspBuild, waspBuildStart } from "../waspCli.ts";

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
