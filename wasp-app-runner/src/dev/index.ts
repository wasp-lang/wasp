import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { createLogger } from "../logging.js";
import { Process } from "../process.js";
import { type AppName, waspMigrateDb, waspStart } from "../waspCli.js";

export async function startAppInDevMode({
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
}): Promise<void> {
  using db = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
  });

  await waspMigrateDb({
    waspCliCmd,
    pathToApp,
    extraEnv: db.dbEnvVars,
  });

  using wasp = await waspStart({
    waspCliCmd,
    pathToApp,
    extraEnv: db.dbEnvVars,
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
