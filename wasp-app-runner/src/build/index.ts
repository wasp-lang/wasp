import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { startLocalSmtpServer } from "../smtp.js";
import type { VersionSettings } from "../versions.js";
import { type AppName, waspBuild } from "../waspCli.js";
import { buildClientApp, startClientApp } from "./client.js";
import { buildServerAppContainer, runServerAppContainer } from "./server.js";

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

  /*
    We do the client and server builds first, in parallel. Then it starts the
    client build, waits until it's up; and only then starts the server build.
    Then it waits for both processes.

    This is because the client needs to be fully started before the server
    starts, as `playwright` tests start executing as soon as the server is up.
  */

  const [, { containerName, imageName }] = await Promise.all([
    buildClientApp({ pathToApp, versionSettings }),
    buildServerAppContainer({
      appName,
      pathToApp,
      versionSettings,
    }),
  ]);

  const { processPromise: startClientProcessPromise } = await startClientApp({
    pathToApp,
    versionSettings,
  });

  const { processPromise: startServerProcessPromise } =
    await runServerAppContainer({
      containerName,
      imageName,
      pathToApp,
      extraEnv: dbEnvVars,
    });

  await Promise.race([startClientProcessPromise, startServerProcessPromise]);
}
