import { Command, Argument } from "@commander-js/extra-typings";
import { Branded } from "./types.js";

export type Mode = "dev" | "build";
export type PathToApp = Branded<string, "PathToApp">;
export type WaspCliCmd = Branded<string, "WaspCliCmd">;

export function parseArgs(): {
  mode: Mode;
  pathToApp: PathToApp;
  waspCliCmd: WaspCliCmd;
} {
  const program = new Command();

  program.name("wasp-app-runner");

  const runCommand = program
    .command("run")
    .description("Run the Wasp application")
    .addArgument(
      new Argument("<mode>", "The run mode").choices(["dev", "build"])
    )
    .option("--path-to-app <path>", "Path to the Wasp application", ".")
    .option("--wasp-cli-cmd <command>", "Wasp CLI command to use", "wasp");

  if (process.argv.length === 2) {
    program.help();
  }

  program.parse();

  const options = runCommand.opts();
  const args = runCommand.processedArgs;

  return {
    mode: args[0],
    pathToApp: options.pathToApp as PathToApp,
    waspCliCmd: options.waspCliCmd as WaspCliCmd,
  };
}
