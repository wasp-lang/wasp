import { execa } from "execa";
import * as streamConsumers from "node:stream/consumers";
import { createLogger, type Logger } from "./logging.js";
import { shutdownSignal } from "./shutdown-controller.js";
import type { EnvVars } from "./types.js";
import * as asyncIterable from "./util/async-iterable.js";

export type SpawnOptions = {
  logger?: Logger;
  cmd: string;
  args: string[];
  cwd?: string;
  env?: EnvVars;
  print?: boolean;
  detached?: boolean;
};

export interface ProcessExit {
  exitCode: number | null;
}

export interface ProcessResult extends ProcessExit {
  stdout: string;
  stderr: string;
}

export class Process {
  #proc;
  #logger;
  #detached;
  #closePromise;

  constructor(options: SpawnOptions) {
    this.#logger = options.logger ?? createLogger("process");
    this.#detached = options.detached ?? false;

    this.#proc = execa(options.cmd, options.args, {
      cwd: options.cwd,
      env: options.env,

      // Close stdin immediately. The Wasp CLI inherits this handle, and some
      // of its child processes (e.g. `prisma db execute --stdin`) read from
      // stdin until EOF. Leaving stdin as the default "pipe" would cause
      // those processes to hang forever waiting for input.
      stdin: "ignore",

      // We don't want execa to throw an error when the process exits with a
      // non-zero code, as we'll handle that ourselves in the wait() method.
      reject: false,

      // Detached mode spawns a process group, allowing us to kill the entire
      // group at once. See the kill() method below for details.
      detached: options.detached ?? false,
    });

    shutdownSignal.addEventListener("abort", () => this.kill());

    const awaitables = [
      this.#proc,
      ...(options.print
        ? [
            asyncIterable.forEach(this.stdoutLines, (line) =>
              this.#logger.info(line),
            ),
            asyncIterable.forEach(this.stderrLines, (line) =>
              this.#logger.error(line),
            ),
          ]
        : []),
    ] as const;

    this.#closePromise = Promise.all(awaitables).then(([resultOrError]) => {
      if (resultOrError.exitCode !== undefined) {
        return { exitCode: resultOrError.exitCode };
      } else if (resultOrError.isTerminated) {
        return { exitCode: null };
      } else {
        throw resultOrError;
      }
    });
  }

  get stdoutLines(): AsyncIterable<string> {
    return this.#proc.iterable({ from: "stdout" });
  }

  get stderrLines(): AsyncIterable<string> {
    return this.#proc.iterable({ from: "stderr" });
  }

  async collect(): Promise<ProcessResult> {
    const [stdout, stderr, { exitCode }] = await Promise.all([
      this.#proc.stdout
        ? streamConsumers.text(this.#proc.stdout)
        : Promise.resolve(""),
      this.#proc.stderr
        ? streamConsumers.text(this.#proc.stderr)
        : Promise.resolve(""),
      this.#closePromise,
    ]);

    return { exitCode, stdout, stderr };
  }

  async wait(): Promise<ProcessExit> {
    const { exitCode } = await this.#closePromise;
    if (exitCode !== 0) {
      this.#logger.fatal(`Process exited with code ${exitCode}`);
    }
    return { exitCode };
  }

  kill(): void {
    // Wasp is somewhat badly behaved in that it doesn't always kill its child
    // processes when it receives a shutdown signal. To work around this, we
    // attempt to kill the process in three different ways, ignoring "process
    // not found" errors.

    // We always use SIGINT as it's the same signal sent from the terminal when
    // pressing Ctrl+C.

    if (this.#detached) {
      // A negative PID kills the entire process group.
      this.#ignoreKillError(() => process.kill(-this.#proc.pid!, "SIGINT"));
    }

    // We kill the main process directly.
    this.#ignoreKillError(() => process.kill(this.#proc.pid!, "SIGINT"));

    // And finally we let execa attempt to kill the process for bookkeeping.
    this.#ignoreKillError(() => this.#proc.kill("SIGINT"));
  }

  #ignoreKillError(fn: () => void) {
    try {
      fn();
    } catch (error: any) {
      if (error.syscall === "kill" && error.code === "ESRCH") {
        return;
      } else {
        throw error;
      }
    }
  }
}
