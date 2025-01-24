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
  })
  .parseSync();

export function getOptions() {
  return {
    pathToApp: argv["app-path"],
    appName: argv["app-name"],
    dbType: argv["db-type"] as DbType,
    skipCliInstall: argv["skip-cli-install"],
  };
}

export type Options = ReturnType<typeof getOptions>;
