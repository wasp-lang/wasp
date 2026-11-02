/// <reference types="node" />

import { ExecaError } from "execa";
import { constants } from "node:os";
import { commandGroups, findCommand } from "./commands.ts";
import { printUsage } from "./usage.ts";

export async function runCli(argv: string[]): Promise<void> {
  const [commandName, ...commandArgs] = argv;

  if (commandName === undefined) {
    printUsage(commandGroups);
    return;
  }

  const command = findCommand(commandName);
  if (!command) {
    printUsage(commandGroups);
    process.exitCode = 1;
    return;
  }

  keepRunningOnInterrupt();
  try {
    await command.run(commandArgs);
  } catch (error) {
    process.exitCode = reportFailure(error);
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
