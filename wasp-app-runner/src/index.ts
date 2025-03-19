import yargs from "yargs/yargs";
import { hideBin } from "yargs/helpers";
import { log } from "./logging.js";
import { checkAndSetupDependencies } from "./dependencies.js";
import { setupEnvFiles } from "./env.js";
import { executeWithDb } from "./db/index.js";
import { getAppInfo, installWaspCli, migrateDb, startApp } from "./waspCli.js";

const argv = yargs(hideBin(process.argv))
  .options({
    "app-path": {
      type: "string",
      description: "Path to the application",
      default: ".",
    },
    "skip-cli-install": {
      type: "boolean",
      default: false,
      description: "Skip installing Wasp CLI from source",
    },
    "wasp-cli-cmd": {
      type: "string",
      description: "Command to use for Wasp CLI",
      default: "wasp-cli",
    },
  })
  .parseSync();

async function main() {
  try {
    await checkAndSetupDependencies({
      isWaspCliBuiltFromSource: !argv.skipCliInstall,
    });

    const { appName, dbType } = await getAppInfo({
      waspCliCmd: argv.waspCliCmd,
      pathToApp: argv.appPath,
    });

    log("setup", "info", `Starting application: ${appName}`);

    setupEnvFiles({
      pathToApp: argv.appPath,
    });

    await executeWithDb(
      {
        appName,
        dbType,
        pathToApp: argv.appPath,
      },
      async ({ extraEnv }) => {
        await migrateDb({
          waspCliCmd: argv.waspCliCmd,
          pathToApp: argv.appPath,
          extraEnv,
        });

        await startApp({
          waspCliCmd: argv.waspCliCmd,
          pathToApp: argv.appPath,
          extraEnv,
        });
      }
    );
  } catch (error: unknown) {
    if (error instanceof Error) {
      log("main", "error", `Fatal error: ${error.message}`);
    } else {
      log("main", "error", `Fatal error: ${error}`);
    }
    process.exit(1);
  }
}

main();
