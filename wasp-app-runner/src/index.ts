import { readdir } from "fs/promises";
import { type Mode, parseArgs, PathToApp, WaspCliCmd } from "./args.js";
import { startAppInBuildMode } from "./build/index.js";
import { checkDependencies } from "./dependencies.js";
import { startAppInDevMode } from "./dev/index.js";
import { createLogger } from "./logging.js";
import { waspInfo, waspTsSetup } from "./waspCli.js";

const logger = createLogger("main");

export async function main(): Promise<void> {
  const { mode, waspCliCmd, pathToApp } = parseArgs();

  try {
    await runWaspApp({
      mode,
      waspCliCmd,
      pathToApp,
    });
  } catch (error: unknown) {
    if (error instanceof Error) {
      logger.error(`Fatal error: ${error.message}`);
    } else {
      logger.error(`Fatal error: ${error}`);
    }
    process.exit(1);
  }
}

async function runWaspApp({
  mode,
  waspCliCmd,
  pathToApp,
}: {
  mode: Mode;
  waspCliCmd: WaspCliCmd;
  pathToApp: PathToApp;
}): Promise<void> {
  await checkDependencies();

  const { appName, dbType } = await waspInfo({
    waspCliCmd,
    pathToApp,
  });

  if (await isWaspTypescriptConfigProject(pathToApp)) {
    await waspTsSetup({
      waspCliCmd,
      pathToApp,
    });
  }

  logger.info(
    `Starting "${appName}" app (mode: ${mode}) using "${waspCliCmd}" command`,
  );

  switch (mode) {
    case "dev":
      await startAppInDevMode({
        waspCliCmd,
        pathToApp,
        appName,
        dbType,
      });
      break;

    case "build":
      await startAppInBuildMode({
        waspCliCmd,
        pathToApp,
        appName,
        dbType,
      });
      break;

    default:
      mode satisfies never;
  }
}

async function isWaspTypescriptConfigProject(pathToApp: PathToApp) {
  try {
    const files = await readdir(pathToApp);
    return files.some((file) => file.endsWith(".wasp.ts"));
  } catch (error) {
    logger.error(`Failed to read directory ${pathToApp}: ${error}`);
    process.exit(1);
  }
}
