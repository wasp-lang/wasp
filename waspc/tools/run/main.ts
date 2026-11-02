/// <reference types="node" />

import { ExecaError } from "execa";
import { constants } from "node:os";
import { styleText } from "node:util";
import { type Command, commandGroups, findCommand } from "./commands.ts";

const [commandName, ...commandArgs] = process.argv.slice(2);
const command = commandName ? findCommand(commandName) : undefined;

if (command) {
  keepRunningOnInterrupt();
  try {
    await command.run(commandArgs);
  } catch (error) {
    process.exitCode = reportFailure(error);
  }
} else {
  printUsage();
  if (commandName) {
    process.exitCode = 1;
  }
}

// The terminal sends Ctrl+C to the child processes too. Staying alive lets
// them shut down cleanly before we exit with their exit code. A second Ctrl+C
// exits right away.
function keepRunningOnInterrupt(): void {
  process.once("SIGINT", () => {
    process.once("SIGINT", () => process.exit(130));
  });
}

function reportFailure(error: unknown): number {
  if (!(error instanceof ExecaError)) {
    console.error(`ERROR: ${getErrorMessage(error)}`);
    return 1;
  }
  if (error.exitCode !== undefined) {
    return error.exitCode;
  }
  if (error.signal !== undefined) {
    return 128 + constants.signals[error.signal];
  }
  console.error(`ERROR: ${error.shortMessage}`);
  return 1;
}

function getErrorMessage(error: unknown): string {
  return error instanceof Error ? error.message : String(error);
}

function printUsage(): void {
  const commandsUsage = commandGroups
    .map((commandGroup) => commandGroup.map(formatCommandUsage).join("\n"))
    .join("\n\n");
  console.log(styleText("bold", "USAGE"));
  console.log("    run <command>\n");
  console.log(styleText("bold", "COMMANDS"));
  console.log(commandsUsage);
}

function formatCommandUsage({ name, argsHint, description }: Command): string {
  const signature = argsHint ? `${name} ${argsHint}` : name;
  const styledSignature = styleText(["yellow", "bold"], signature);
  return `    ${styledSignature}\n        ${description}`;
}
