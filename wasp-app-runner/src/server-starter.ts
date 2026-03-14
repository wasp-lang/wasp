import { Logger } from "./logging.js";
import { Process } from "./process.js";

export interface Server<T = unknown> extends Disposable {
  proc: Process;
  value: T;
}

export async function startServer<T>(
  logger: Logger,
  {
    cmd,
    args,
    cwd,
    env,
  }: {
    cmd: string;
    args: string[];
    cwd?: string;
    env?: Record<string, string>;
  },
  checkFn: (proc: Process) => Promise<T>,
): Promise<Server<T>> {
  const proc = new Process({ logger, cmd, args, cwd, env, print: true, detached: true });

  try {
    const value = await Promise.race([
      checkFn(proc),
      proc
        .wait()
        .then((value) =>
          logger.fatal(
            `Process exited unexpectedly with code ${value.exitCode}`,
          ),
        ),
    ]);

    return {
      proc,
      value,
      [Symbol.dispose]: () => proc.kill(),
    };
  } catch (error) {
    proc.kill();
    throw error;
  }
}
