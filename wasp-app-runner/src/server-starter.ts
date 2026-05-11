import { Logger } from "./logging.js";
import { Process } from "./process.js";

export interface Server extends Disposable {
  proc: Process;
}

export async function startServer(
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
  checkFn: (proc: Process) => Promise<void>,
): Promise<Server> {
  const proc = new Process({
    logger,
    cmd,
    args,
    cwd,
    env,
    print: true,
    detached: true,
  });

  try {
    await Promise.race([
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
      [Symbol.dispose]: () => proc.kill(),
    };
  } catch (error) {
    proc.kill();
    throw error;
  }
}
