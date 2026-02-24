import { readdir } from "fs/promises";
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
import { createLogger } from "./logging.js";
import { CLIError } from "./logging.js";
import { childProcessManager } from "./process.js";
import { waspInfo, waspTsSetup } from "./waspCli.js";

const logger = createLogger("main");

export async function main(): Promise<void> {
  const { mode, waspCliCmd, pathToApp, dbImage } = parseArgs();

  const abortController = new AbortController();
  const { signal } = abortController;

  const onSignal = () => abortController.abort();
  process.on("SIGINT", onSignal);
  process.on("SIGTERM", onSignal);

  childProcessManager.connectSignal(signal);

  try {
    await runWaspApp({ mode, waspCliCmd, pathToApp, dbImage, signal });
  } catch (error: unknown) {
    if (signal.aborted) {
      process.exitCode = 1;
      return;
    }
    if (error instanceof CLIError) {
      const errorLogger = createLogger(error.processName);
      errorLogger.error(error.message);
      process.exitCode = 1;
      return;
    }
    if (error instanceof Error) {
      logger.error(`Fatal error: ${error.message}`);
    } else {
      logger.error(`Fatal error: ${error}`);
    }
    process.exitCode = 1;
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
  await checkDependencies();

  const { appName, dbType } = await waspInfo({
    waspCliCmd,
    pathToApp,
  });

  if (dbImageArg && dbType !== DbType.Postgres) {
    throw logger.cliError(
      "The --db-image option is only valid when using PostgreSQL as the database.",
    );
  }
  const dbImage = dbImageArg ?? defaultPostgresDbImage;

  if (await isWaspTypescriptConfigProject(pathToApp)) {
    await waspTsSetup({
      waspCliCmd,
      pathToApp,
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
    throw logger.cliError(
      `Failed to read directory ${pathToApp}: ${error}`,
    );
  }
}
