import { log } from "./logging.js";
import { getOptions } from "./cli.js";
import { checkDependencies } from "./dependencies.js";
import { processManager } from "./process.js";
import { setupEnvFiles } from "./env.js";
import { getWaspcDirAbsPath } from "./path.js";
import { executeWithDb } from "./db/index.js";

const options = getOptions();

async function installWaspCli() {
  log("install-wasp-cli", "info", "Installing Wasp CLI globally...");

  return processManager.spawnWithLog({
    name: "install-wasp-cli",
    cmd: "cabal",
    args: ["install", "--overwrite-policy=always"],
    cwd: getWaspcDirAbsPath(),
  });
}

async function main() {
  try {
    log("setup", "info", `Starting application: ${options.appName}`);

    await checkDependencies();

    await setupEnvFiles(options);

    if (!options.skipCliInstall) {
      await installWaspCli();
    }

    await executeWithDb(options, async ({ extraEnv }) => {
      await processManager.spawnWithLog({
        name: "migrate-db",
        cmd: options.waspCliCmd,
        args: ["db", "migrate-dev"],
        cwd: options.pathToApp,
        extraEnv,
      });

      await processManager.spawnWithLog({
        name: "start-app",
        cmd: options.waspCliCmd,
        args: ["start"],
        cwd: options.pathToApp,
        extraEnv,
      });
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

main();
