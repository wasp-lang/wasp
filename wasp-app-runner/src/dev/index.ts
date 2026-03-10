import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { waitUntilAppReady } from "../http.js";
import { Process } from "../process.js";
import { shutdownPromise } from "../shutdown.js";
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
  await using db = await setupDb({
    appName,
    dbType,
    pathToApp,
    dbImage,
  });

  const { dbEnvVars } = await db.waitUntilReady();

  await waspMigrateDb({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
  });

  const wasp = waspStart({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
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
