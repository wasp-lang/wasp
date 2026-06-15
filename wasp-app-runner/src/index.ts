import { readdir } from "node:fs/promises";
import {
  parseArgs,
  type DockerImageName,
  type Mode,
  type PathToApp,
  type WaspCliCmd,
} from "./args.js";
import { startAppInBuildMode } from "./build/index.js";
import { DbType } from "./db/index.js";
import { defaultPostgresDbImage } from "./db/postgres.js";
import { checkDependencies } from "./dependencies.js";
import { startAppInDevMode } from "./dev/index.js";
import { createLogger, LoggerError, reportFatalError } from "./logging.js";
import { installShutdownHandlers } from "./shutdown.js";
import { waspInfo, waspInstall } from "./waspCli.js";

const logger = createLogger("main");

export async function main(): Promise<void> {
  // `commander` may call `process.exit` on `--help` or invalid arguments. That
  // happens before we own any resources, so it's acceptable here.
  const { mode, waspCliCmd, pathToApp, dbImage } = parseArgs();

  const controller = new AbortController();
  using _shutdown = installShutdownHandlers(controller);
  const { signal } = controller;

  try {
    await runWaspApp({
      mode,
      waspCliCmd,
      pathToApp,
      dbImage,
      signal,
    });
  } catch (error: unknown) {
    if (signal.aborted) {
      // Graceful shutdown: teardown already ran via `await using` unwinding.
      logger.warn(`Shutting down: ${errorMessage(error)}`);
    } else if (error instanceof LoggerError) {
      reportFatalError(error);
      process.exitCode = 1;
    } else {
      logger.error(`Fatal error: ${errorMessage(error)}`);
      process.exitCode = 1;
    }
  }
}

async function runWaspApp({
  mode,
  waspCliCmd,
  pathToApp,
  dbImage: dbImageArg,
  signal,
}: {
  mode: Mode;
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
  dbImage?: DockerImageName;
  signal: AbortSignal;
}): Promise<void> {
  await checkDependencies({ signal });

  const { appName, dbType } = await waspInfo({
    waspCliCmd,
    pathToApp,
    signal,
  });

  if (dbImageArg && dbType !== DbType.Postgres) {
    logger.fatal(
      "The --db-image option is only valid when using PostgreSQL as the database.",
    );
  }
  const dbImage = dbImageArg ?? defaultPostgresDbImage;

  if (await isWaspTypescriptConfigProject(pathToApp)) {
    await waspInstall({
      waspCliCmd,
      pathToApp,
      signal,
    });
  }

  logger.info(
    `Starting "${appName}" app (mode: ${mode}) using "${[waspCliCmd.cmd, ...waspCliCmd.args].join(" ")}" command`,
  );

  switch (mode) {
    case "dev":
      await startAppInDevMode({
        waspCliCmd,
        pathToApp,
        appName,
        dbType,
        dbImage,
        signal,
      });
      break;

    case "build":
      await startAppInBuildMode({
        waspCliCmd,
        pathToApp,
        appName,
        dbType,
        dbImage,
        signal,
      });
      break;

    default:
      mode satisfies never;
  }
}

async function isWaspTypescriptConfigProject(
  pathToApp: PathToApp,
): Promise<boolean> {
  try {
    const files = await readdir(pathToApp);
    return files.some((file) => file.endsWith(".wasp.ts"));
  } catch (error) {
    return logger.fatal(`Failed to read directory ${pathToApp}`, {
      cause: error,
    });
  }
}

function errorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}
