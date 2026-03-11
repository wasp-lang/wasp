import * as path from "path";
import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { doesFileExist } from "../files.js";
import { createLogger } from "../logging.js";
import { Process } from "../process.js";
import { startLocalSmtpServer } from "../smtp.js";
import { EnvVars } from "../types.js";
import { type AppName, waspBuild, waspBuildStart } from "../waspCli.js";

export async function startAppInBuildMode({
  waspCliCmd,
  pathToApp,
  appName,
  dbType,
  dbImage,
  run,
  exit,
}: {
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  appName: AppName;
  dbType: DbType;
  dbImage: DockerImageName;
  run?: string;
  exit: boolean;
}) {
  await waspBuild({
    waspCliCmd,
    pathToApp,
  });

  using db = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
  });

  using _smtp = await startLocalSmtpServer();

  const serverEnvVars: EnvVars = {
    JWT_SECRET: "some-jwt-secret",
    ...db.dbEnvVars,
  };

  const serverEnvFile = path.resolve(pathToApp, ".env.server");
  const clientEnvFile = path.resolve(pathToApp, ".env.client");

  using wasp = await waspBuildStart({
    waspCliCmd,
    pathToApp,
    serverEnvVars,
    serverEnvFile: doesFileExist(serverEnvFile) ? serverEnvFile : undefined,
    clientEnvFile: doesFileExist(clientEnvFile) ? clientEnvFile : undefined,
  });

  if (run) {
    await new Process({
      logger: createLogger("run"),
      cmd: process.env.SHELL ?? "sh",
      args: ["-c", run],
      env: {
        client_url: "http://localhost:3000",
        server_url: "http://localhost:3001",
      },
    })
      .print()
      .wait();
  }

  if (exit) {
    return;
  } else {
    await wasp.proc.wait();
  }
}
