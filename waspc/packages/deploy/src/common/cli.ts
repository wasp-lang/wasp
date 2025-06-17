import { $, type ProcessPromise, type Options as ZxOptions } from "zx";

export function createCommandWithDirectory(
  command: string,
  cwd: string,
): (commandArgs: string[], options?: Partial<ZxOptions>) => ProcessPromise {
  return (
    commandArgs: string[],
    options?: Partial<ZxOptions>,
  ): ProcessPromise => {
    return $({
      cwd,
      ...(options ?? {}),
    })`${command} ${commandArgs}`;
  };
}
