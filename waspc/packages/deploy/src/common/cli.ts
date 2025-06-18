import { $, type ProcessPromise, type Options as ZxOptions } from "zx";

export function createCommandWithDirectory(
  command: string,
  workingDirectoryPath: string,
): (commandArgs: string[], options?: Partial<ZxOptions>) => ProcessPromise {
  return (
    commandArgs: string[],
    options?: Partial<ZxOptions>,
  ): ProcessPromise => {
    return $({
      cwd: workingDirectoryPath,
      ...(options ?? {}),
    })`${command} ${commandArgs}`;
  };
}
