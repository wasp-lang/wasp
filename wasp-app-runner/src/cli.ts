import yargs from "yargs/yargs";
import { hideBin } from "yargs/helpers";
import { type DbType, dbTypes } from "./db/index.js";

const argv = yargs(hideBin(process.argv))
  .options({
    "app-path": {
      type: "string",
      description: "Path to the application",
      demandOption: true,
    },
    "app-name": {
      type: "string",
      description: "Name of the application (used for DB container name)",
      demandOption: true,
    },
    "db-type": {
      choices: dbTypes,
      description: "Database type",
      default: "postgres",
    },
    "skip-cli-install": {
      type: "boolean",
      default: false,
      description: "Skip installing Wasp CLI globally",
    },
    "wasp-cli-cmd": {
      type: "string",
      description: "Command to use for Wasp CLI",
      default: "wasp-cli",
    },
  })
  .check((argv) => {
    const appName = argv["app-name"];
    const validNameRegex = /^[a-zA-Z0-9][a-zA-Z0-9_.-]*$/;
    if (!validNameRegex.test(appName)) {
      throw new Error(
        `Invalid app name: ${appName}. It should only contain alphanumeric characters, dots, underscores, and dashes, and must start with an alphanumeric character.`
      );
    }
    return true;
  })
  .parseSync();

export function getOptions() {
  return {
    pathToApp: argv["app-path"],
    appName: argv["app-name"],
    dbType: argv["db-type"] as DbType,
    skipCliInstall: argv["skip-cli-install"],
    waspCliCmd: argv["wasp-cli-cmd"],
  };
}

export type Options = ReturnType<typeof getOptions>;
