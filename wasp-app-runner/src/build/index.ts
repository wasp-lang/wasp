import type { DockerImageName, PathToApp, WaspCliCmd } from "../args.js";
import { DbType, setupDb } from "../db/index.js";
import { startLocalSmtpServer } from "../smtp.js";
import { type AppName, waspBuild } from "../waspCli.js";
import { buildClientApp, startClientApp } from "./client.js";
import { buildServerApp, startServerApp } from "./server.js";
import { createLogger } from "../logging.js";

const logger = createLogger("build");

// Based on https://github.com/wasp-lang/wasp/issues/1883#issuecomment-2766265289
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

  // Build both client and server in parallel
  await Promise.all([
    buildClientApp({ pathToApp }),
    buildServerApp({ appName, pathToApp }),
  ]);

  logger.info("Both client and server builds completed. Starting applications...");

  // Start both client and server (client needs to be running before the server
  // because `playwright` tests start executing as soon as the server is up)
  startClientApp({ pathToApp }).catch((error) => {
    logger.error(`Failed to start client app with exit code: ${error.exitCode ?? 'unknown'}`);
    process.exit(1);
  });

  startServerApp({
    appName,
    pathToApp,
    extraEnv: dbEnvVars,
  });

  // Keep the process alive indefinitely while child processes are running.
  // The ChildProcessManager will handle cleanup on SIGINT/SIGTERM.
  await new Promise(() => {});
}
