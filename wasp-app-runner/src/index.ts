import yargs from "yargs/yargs";
import { hideBin } from "yargs/helpers";
import { log } from "./logging.js";
import { checkDependencies } from "./dependencies.js";
import { setupDb } from "./db/index.js";
import { getAppInfo, migrateDb, startApp } from "./waspCli.js";

export async function main(): Promise<void> {
  const { waspCliCmd, pathToApp } = parseArgs();

  try {
    await runWaspApp({
      waspCliCmd,
      pathToApp,
    });
  } catch (error: unknown) {
    if (error instanceof Error) {
      log("main", "error", `Fatal error: ${error.message}`);
    } else {
      log("main", "error", `Fatal error: ${error}`);
    }
    process.exit(1);
  }
}

async function runWaspApp({
  waspCliCmd,
  pathToApp,
}: {
  waspCliCmd: string;
  pathToApp: string;
}): Promise<void> {
  await checkDependencies();

  const { appName, dbType } = await getAppInfo({
    waspCliCmd,
    pathToApp,
  });

  log(
    "setup",
    "info",
    `Starting "${appName}" app using "${waspCliCmd}" command`
  );

  const { dbEnvVars } = await setupDb({
    appName,
    dbType,
    pathToApp,
  });

  await migrateDb({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
  });

  await startApp({
    waspCliCmd,
    pathToApp,
    extraEnv: dbEnvVars,
  });
}

function parseArgs(): {
  pathToApp: string;
  waspCliCmd: string;
} {
  const argv = yargs(hideBin(process.argv))
    .options({
      "path-to-app": {
        type: "string",
        description: "Path to the application",
        default: ".",
      },
      "wasp-cli-cmd": {
        type: "string",
        description: "Command to use for Wasp CLI ",
        default: "wasp",
      },
    })
    .strict()
    .parseSync();

  return {
    pathToApp: argv["path-to-app"],
    waspCliCmd: argv["wasp-cli-cmd"],
  };
}
