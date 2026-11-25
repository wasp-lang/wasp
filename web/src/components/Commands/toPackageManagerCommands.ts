import { type PackageManagerCommands } from "./packageManagers";

/** The Wasp that runs a command, the project's own or the global Wasp CLI. */
export type WaspCli = "project" | "global";

/**
 * Writes a command out for each package manager. Authors write it for the
 * global Wasp CLI and npm, e.g. `npm install` and then `wasp start`.
 *
 * The lines that run `wasp` follow `waspCli`. For the project's Wasp they go
 * through the package manager, e.g. `npm run dev`, and the global Wasp CLI
 * keeps `wasp start`. The package manager lines follow the package manager,
 * and every other line stays as it is.
 */
export function toPackageManagerCommands(
  command: string,
  waspCli: WaspCli,
): PackageManagerCommands {
  const commandLines = command.split("\n");
  return {
    // Authors write the package manager lines for npm, so npm keeps them.
    // Another package manager has to rewrite them here, for both Wasp CLIs.
    npm: commandLines
      .map((commandLine) =>
        waspCli === "project" ? toNpmCommandLine(commandLine) : commandLine,
      )
      .join("\n"),
  };
}

export function runsWasp(command: string): boolean {
  return command
    .split("\n")
    .some((commandLine) => WASP_COMMAND_LINE.test(commandLine));
}

// Environment variables set for the command, then the arguments of `wasp`,
// e.g. `DATABASE_URL=... wasp db studio`.
const WASP_COMMAND_LINE = /^((?:\w+=\S+ )*)wasp (.+)$/;

function toNpmCommandLine(commandLine: string): string {
  const waspCommandLine = commandLine.match(WASP_COMMAND_LINE);
  if (!waspCommandLine) {
    return commandLine;
  }
  const [, envVars, waspArgs] = waspCommandLine;
  return envVars + toNpmCommand(waspArgs.split(" "));
}

/**
 * Uses the npm scripts that Wasp projects come with. `dev`, `build` and
 * `preview` run their Wasp commands, and `wasp` runs every other one.
 */
function toNpmCommand(waspArgs: string[]): string {
  const [command, subcommand] = waspArgs;
  if (command === "new") {
    // There's no project yet, so no npm script to run.
    return ["npx @wasp/cli@latest", ...waspArgs].join(" ");
  }
  if (command === "build" && subcommand === "start") {
    return runNpmScript("preview", waspArgs.slice(2));
  }
  if (command === "build") {
    return runNpmScript("build", waspArgs.slice(1));
  }
  if (command === "start" && subcommand !== "db") {
    return runNpmScript("dev", waspArgs.slice(1));
  }
  return runNpmScript("wasp", waspArgs);
}

/** npm takes the script's own arguments after `--`. */
function runNpmScript(scriptName: string, scriptArgs: string[]): string {
  const npmArgs = scriptArgs.length > 0 ? ["--", ...scriptArgs] : [];
  return ["npm run", scriptName, ...npmArgs].join(" ");
}
