/// <reference types="node" />

import { execa } from "execa";
import { styleText } from "node:util";

export async function runCmd(
  cmd: string,
  args: string[],
  options: { cwd: string; env?: Record<string, string> },
): Promise<void> {
  logRunning(cmd, args);
  await execa(cmd, args, { ...options, stdio: "inherit" });
}

// Long argument lists are shortened in the log.
const MAX_LOGGED_ARGS = 25;

function logRunning(cmd: string, args: string[]): void {
  const label = styleText("cyanBright", "Running:", { stream: process.stderr });
  process.stderr.write(`${label} ${formatCmdLine(cmd, args)} \n\n`);
}

function formatCmdLine(cmd: string, args: string[]): string {
  const shownArgs = args.slice(0, MAX_LOGGED_ARGS).map(quoteIfHasWhitespace);
  const omittedArgsCount = args.length - shownArgs.length;
  const cmdLine = [cmd, ...shownArgs].join(" ");
  return omittedArgsCount > 0
    ? `${cmdLine} … ${omittedArgsCount} more`
    : cmdLine;
}

function quoteIfHasWhitespace(arg: string): string {
  return /\s/.test(arg) ? JSON.stringify(arg) : arg;
}
