import * as path from "path";
import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { doesFileExist } from "../files.js";
import { waitUntilAppReady } from "../http.js";
import { Process } from "../process.js";
import { shutdownPromise } from "../shutdown.js";
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

  await using db = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
  });
  const { dbEnvVars } = await db.waitUntilReady();

  await using _smtp = startLocalSmtpServer();

  const serverEnvVars: EnvVars = {
    JWT_SECRET: "some-jwt-secret",
    ...dbEnvVars,
  };

  const serverEnvFile = path.resolve(pathToApp, ".env.server");
  const clientEnvFile = path.resolve(pathToApp, ".env.client");

  const wasp = waspBuildStart({
    waspCliCmd,
    pathToApp,
    serverEnvVars,
    serverEnvFile: doesFileExist(serverEnvFile) ? serverEnvFile : undefined,
    clientEnvFile: doesFileExist(clientEnvFile) ? clientEnvFile : undefined,
  });
  await using _wasp = wasp.disposable();

  await Promise.all([
    waitUntilAppReady({ port: 3000 }),
    waitUntilAppReady({ port: 3001 }),
  ]);

  if (run) {
    await new Process({
      cmd: process.env.SHELL ?? "sh",
      args: ["-c", run],
      env: {
        client_url: "http://localhost:3000",
        server_url: "http://localhost:3001",
      },
    })
      .log("run")
      .wait();
  }

  if (exit) return;

  await Promise.race([wasp.wait(), shutdownPromise]);
}
