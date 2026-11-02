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

function logRunning(cmd: string, args: string[]): void {
  const label = styleText("cyanBright", "Running:", { stream: process.stderr });
  const cmdLine = [cmd, ...args].map(quoteIfHasWhitespace).join(" ");
  process.stderr.write(`${label} ${cmdLine} \n\n`);
}

function quoteIfHasWhitespace(arg: string): string {
  return /\s/.test(arg) ? JSON.stringify(arg) : arg;
}
