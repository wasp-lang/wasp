import { Argument, program } from "@commander-js/extra-typings";
import packageJson from "../package.json" with { type: "json" };
import { defaultPostgresDbImage } from "./db/postgres.js";
import { Branded } from "./types.js";

export type Mode = "dev" | "build";
export type PathToApp = Branded<string, "PathToApp">;
export type WaspCliCmd = Branded<string, "WaspCliCmd">;
export type DockerImageName = Branded<string, "DockerImageName">;

export function parseArgs(): {
  mode: Mode;
  pathToApp: PathToApp;
  waspCliCmd: WaspCliCmd;
  dbImage?: DockerImageName;
} {
  const parsedProgram = program
    .name("run-wasp-app")
    .description("Run the Wasp application")
    .version(packageJson.version)
    .addArgument(
      new Argument("mode", "The run mode")
        .argRequired()
        .choices(["dev", "build"]),
    )
    .option("--path-to-app <path>", "Path to the Wasp application", ".")
    .option("--wasp-cli-cmd <command>", "Wasp CLI command to use", "wasp")
    .option(
      "--db-image <image>",
      `Custom PostgreSQL Docker image to use (default: "${defaultPostgresDbImage}")`,
    )
    .parse();

  const options = parsedProgram.opts();
  const [mode] = parsedProgram.processedArgs;

  return {
    mode,
    pathToApp: options.pathToApp as PathToApp,
    waspCliCmd: options.waspCliCmd as WaspCliCmd,
    dbImage: options.dbImage as DockerImageName,
  };
}
