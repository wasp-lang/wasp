import { $ } from "execa";
import { text } from "node:stream/consumers";
import * as iterable from "./iterable.js";
import { createLogger, type Logger } from "./logging.js";
import { shutdownSignal } from "./shutdown.js";
import type { EnvVars } from "./types.js";

export type SpawnOptions = {
  logger?: Logger;
  cmd: string;
  args: string[];
  cwd?: string;
  env?: EnvVars;
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
  #closePromise;

  constructor(options: SpawnOptions) {
    this.#logger = options.logger ?? createLogger("process");

    this.#proc = $(options.cmd, options.args, {
      cwd: options.cwd,
      env: options.env,
      detached: true,
      reject: false,
    });

    shutdownSignal.addEventListener("abort", () => this.kill());

    this.#closePromise = this.#proc.then((resultOrError) => {
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

  print(): this {
    iterable.forEach(this.stdoutLines, (line) => this.#logger.info(line));
    iterable.forEach(this.stderrLines, (line) => this.#logger.error(line));
    return this;
  }

  async collect(): Promise<ProcessResult> {
    const [stdout, stderr, { exitCode }] = await Promise.all([
      this.#proc.stdout ? text(this.#proc.stdout) : Promise.resolve(""),
      this.#proc.stderr ? text(this.#proc.stderr) : Promise.resolve(""),
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

    // A negative PID kills the entire process group.
    this.#ignoreKillError(() => process.kill(-this.#proc.pid!, "SIGINT"));

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
