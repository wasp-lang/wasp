import yargs from "yargs/yargs";
import { hideBin } from "yargs/helpers";
import { log } from "./logging.js";
import { checkAndSetupDependencies } from "./dependencies.js";
import { setupEnvFiles } from "./env.js";
import { setupDb } from "./db/index.js";
import { getAppInfo, migrateDb, startApp } from "./waspCli.js";

export async function main() {
  const { buildWaspCli, waspCliCmd, pathToApp } = parseArgs();

  try {
    await checkAndSetupDependencies({
      buildWaspCli,
    });

    const { appName, dbType } = await getAppInfo({
      waspCliCmd,
      pathToApp,
    });

    log("setup", "info", `Starting application: ${appName}`);

    setupEnvFiles({
      pathToApp,
    });

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
  } catch (error: unknown) {
    if (error instanceof Error) {
      log("main", "error", `Fatal error: ${error.message}`);
    } else {
      log("main", "error", `Fatal error: ${error}`);
    }
    process.exit(1);
  }
}

function parseArgs(): {
  pathToApp: string;
  buildWaspCli: boolean;
  waspCliCmd: string;
} {
  const argv = yargs(hideBin(process.argv))
    .options({
      "path-to-app": {
        type: "string",
        description: "Path to the application",
        default: ".",
      },
      "build-wasp-cli": {
        type: "boolean",
        default: true,
        description: "Build Wasp CLI from source",
      },
      "wasp-cli-cmd": {
        type: "string",
        description:
          "Command to use for Wasp CLI (only used when not building from source)",
        default: "wasp-cli",
      },
    })
    .strict()
    .parseSync();

  const buildWaspCli = argv["build-wasp-cli"];

  return {
    pathToApp: argv["path-to-app"],
    buildWaspCli,
    // When building from source, always use "wasp-cli", otherwise use the user-provided command
    waspCliCmd: buildWaspCli ? "wasp-cli" : argv["wasp-cli-cmd"],
  };
}
