import { type PackageManagerCommands } from "./packageManagers";

/** The Wasp that runs a command: the project's own, or the global Wasp CLI. */
export type WaspCli = "project" | "global";

/**
 * Writes a command out for each package manager. Authors write it for the
 * global Wasp CLI and npm, e.g. `npm install` and then `wasp start`.
 *
 * The lines that run `wasp` follow `waspCli`: the project's Wasp runs through
 * the package manager, e.g. `npm run dev`, and the global Wasp CLI keeps
 * `wasp start`. The lines that run npm follow the package manager for either
 * Wasp CLI, and every other line stays.
 */
export function toPackageManagerCommands(
  command: string,
  waspCli: WaspCli,
): PackageManagerCommands {
  const npmCommand = command
    .split("\n")
    .map((commandLine) =>
      waspCli === "project" ? toNpmCommandLine(commandLine) : commandLine,
    )
    .join("\n");
  return fromNpmCommand(npmCommand);
}

/**
 * Writes a command of npm out for each package manager. The lines that run
 * `npm` or `npx` change, every other line stays.
 */
export function fromNpmCommand(npmCommand: string): PackageManagerCommands {
  const npmCommandLines = npmCommand.split("\n");
  return {
    npm: npmCommand,
    pnpm: npmCommandLines.map(toPnpmCommandLine).join("\n"),
    bun: npmCommandLines.map(toBunCommandLine).join("\n"),
  };
}

export function runsWasp(command: string): boolean {
  return command
    .split("\n")
    .some((commandLine) => WASP_COMMAND_LINE.test(commandLine));
}

// Environment variables set for the command (e.g. `DATABASE_URL=... wasp db
// studio`), then the arguments of `wasp`.
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
 * Uses the npm scripts that Wasp projects come with: `dev`, `build`, `preview`
 * and `wasp`, which runs every other command.
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

// Environment variables set for the command, then `npm` or `npx` and its
// arguments.
const NPM_COMMAND_LINE = /^((?:\w+=\S+ )*)(npm|npx) (.+)$/;

/** How a package manager spells what npm does. */
interface PackageManagerSpelling {
  name: string;
  /** Runs a package without installing it, like `npx`. */
  runPackage: string;
  devFlag: string;
  /** How it runs the `test` script with arguments. */
  test: string;
}

const toPnpmCommandLine = makeCommandLineTranslator({
  name: "pnpm",
  runPackage: "pnpm dlx",
  devFlag: "--save-dev",
  test: "pnpm test",
});

const toBunCommandLine = makeCommandLineTranslator({
  name: "bun",
  runPackage: "bunx",
  devFlag: "--dev",
  test: "bun run test",
});

function makeCommandLineTranslator(
  spelling: PackageManagerSpelling,
): (npmCommandLine: string) => string {
  return (npmCommandLine) => {
    const npmCommandLineMatch = npmCommandLine.match(NPM_COMMAND_LINE);
    if (!npmCommandLineMatch) {
      return npmCommandLine;
    }
    const [, envVars, tool, args] = npmCommandLineMatch;
    const command =
      tool === "npx"
        ? [spelling.runPackage, args]
        : fromNpmArgs(spelling, args.split(" "));
    return envVars + command.join(" ");
  };
}

function fromNpmArgs(
  spelling: PackageManagerSpelling,
  [npmSubcommand, ...npmArgs]: string[],
): string[] {
  switch (npmSubcommand) {
    case "install":
    case "i": {
      const packageSpecifiers = npmArgs.filter((arg) => !arg.startsWith("-"));
      return packageSpecifiers.length === 0
        ? [spelling.name, "install"]
        : [spelling.name, "add", ...fromNpmInstallArgs(spelling, npmArgs)];
    }
    case "uninstall":
      return [
        spelling.name,
        "remove",
        ...fromNpmInstallArgs(spelling, npmArgs),
      ];
    case "ci":
      return [spelling.name, "install", "--frozen-lockfile"];
    case "run":
      return [spelling.name, "run", ...withoutArgsSeparator(npmArgs)];
    case "test":
      return [spelling.test, ...withoutArgsSeparator(npmArgs)];
    default:
      throw new Error(
        `Don't know how to write \`npm ${npmSubcommand}\` for ${spelling.name}. Teach it to toPackageManagerCommands.ts.`,
      );
  }
}

function fromNpmInstallArgs(
  spelling: PackageManagerSpelling,
  npmArgs: string[],
): string[] {
  return npmArgs.map((arg) => {
    switch (arg) {
      case "-D":
      case "--save-dev":
        return spelling.devFlag;
      case "-g":
        return "--global";
      default:
        return arg;
    }
  });
}

/** Only npm takes the script's own arguments after `--`. */
function withoutArgsSeparator(npmArgs: string[]): string[] {
  return npmArgs.filter((arg) => arg !== "--");
}
