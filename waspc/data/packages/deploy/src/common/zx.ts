import * as z from "zod";
import {
  $,
  type ProcessOutput,
  type ProcessPromise,
  type Options as ZxOptions,
} from "zx";

export type Command = (
  commandArgs: string[],
  options?: Partial<ZxOptions>,
) => ProcessPromise;

export function createCommand(
  command: string,
  defaultOptions: Partial<ZxOptions> = {},
): Command {
  return (commandArgs, options) => {
    return $({ ...defaultOptions, ...options })`${command} ${commandArgs}`;
  };
}

export function createCommandWithCwd(command: string, cwd: string): Command {
  return createCommand(command, { cwd });
}

export async function runJsonCommand<Schema extends z.ZodType>(
  command: Command,
  commandArgs: string[],
  schema: Schema,
): Promise<z.infer<Schema>> {
  const result = await command(commandArgs, { verbose: false });
  return schema.parse(parseJsonFromStdout(result));
}

export async function tryRunJsonCommand<Schema extends z.ZodType>(
  command: Command,
  commandArgs: string[],
  schema: Schema,
): Promise<z.infer<Schema> | null> {
  const result = await command(commandArgs, { verbose: false, nothrow: true });
  if (result.exitCode !== 0) {
    return null;
  }
  return schema.parse(parseJsonFromStdout(result));
}

// zx's `ProcessOutput.json()` parses the combined stdout and stderr, but CLIs
// write progress messages to stderr (e.g. `railway add --json` echoes the
// prompt answers there), which would corrupt the JSON.
// Reported to zx: https://github.com/google/zx/issues/1505
function parseJsonFromStdout(output: ProcessOutput): unknown {
  return JSON.parse(output.stdout);
}
