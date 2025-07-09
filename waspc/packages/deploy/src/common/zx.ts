import { $, type ProcessPromise, type Options as ZxOptions } from "zx";

type Command = (
  commandArgs: string[],
  options?: Partial<ZxOptions>,
) => ProcessPromise;

export function createCommandWithCwd(command: string, cwd: string): Command {
  return (commandArgs, options) => {
    return $({
      cwd,
      ...(options ?? {}),
    })`${command} ${commandArgs}`;
  };
}
