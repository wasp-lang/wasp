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
      detached: this.#detached,
    });

    // Scope the shutdown listener to this process's lifetime so it's removed
    // automatically when the child exits. Otherwise the listener would keep
    // calling kill() on a stale PID, which the OS may have reused.
    const lifetimeController = new AbortController();
    shutdownSignal.addEventListener("abort", () => this.kill(), {
      signal: lifetimeController.signal,
    });

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

    this.#closePromise = Promise.all(awaitables)
      .then(([resultOrError]) => {
        if (resultOrError.exitCode !== undefined) {
          return { exitCode: resultOrError.exitCode };
        } else if (resultOrError.isTerminated) {
          return { exitCode: null };
        } else {
          throw resultOrError;
        }
      })
      .finally(() => lifetimeController.abort());
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
    // A null exitCode means the process was terminated by a signal, which
    // happens during graceful shutdown (e.g. Ctrl+C). Don't treat that as an
    // error, since the termination was requested.
    if (exitCode !== 0 && exitCode !== null) {
      this.#logger.fatal(`Process exited with code ${exitCode}`);
    }
    return { exitCode };
  }

  kill(): void {
    const pid = this.#proc.pid;
    if (pid === undefined) {
      return;
    }

    // Send a single SIGINT, matching what the terminal sends on Ctrl+C.
    // Sending the signal more than once breaks `docker run --rm`, which
    // skips container cleanup once it receives 3 signals in quick
    // succession ("got 3 SIGTERM/SIGINTs, forcefully exiting") and leaks
    // the container. Some parents (e.g. the Wasp CLI) don't forward signals
    // to their children, so for detached processes we signal the entire
    // process group (negative PID).
    this.#ignoreKillError(() => {
      if (this.#detached) {
        process.kill(-pid, "SIGINT");
      } else {
        process.kill(pid, "SIGINT");
      }
    });
  }

  #ignoreKillError(fn: () => void) {
    try {
      fn();
    } catch (error: unknown) {
      if (isNoSuchProcessError(error)) {
        return;
      }
      throw error;
    }
  }
}

function isNoSuchProcessError(error: unknown): boolean {
  return (
    error instanceof Error &&
    "syscall" in error &&
    error.syscall === "kill" &&
    "code" in error &&
    error.code === "ESRCH"
  );
}
