import { spawn } from "child_process";
import { text } from "node:stream/consumers";
import readline from "readline";
import { CLIError, createLogger, type Logger } from "./logging.js";
import { shutdownSignal } from "./shutdown.js";
import type { EnvVars } from "./types.js";

export type SpawnOptions = {
  cmd: string;
  args: string[];
  cwd?: string;
  env?: EnvVars;
};

export type ProcessResult = {
  exitCode: number | null;
  stdout: string;
  stderr: string;
};

export class Process {
  #proc: ReturnType<typeof spawn>;
  #logger?: Logger;
  #closePromise: Promise<number | null>;

  constructor(options: SpawnOptions) {
    this.#proc = spawn(options.cmd, options.args, {
      cwd: options.cwd,
      env: options.env ? { ...process.env, ...options.env } : undefined,
      signal: shutdownSignal,
      stdio: ["ignore", "pipe", "pipe"],
    });

    this.#closePromise = new Promise<number | null>((resolve, reject) => {
      this.#proc.on("close", (code) => resolve(code));
      this.#proc.on("error", (err) => {
        if (err.name === "AbortError") return;
        reject(err);
      });
    });
  }

  log(name: string): this {
    this.#logger = createLogger(name);
    const logger = this.#logger;

    if (this.#proc.stdout) {
      const stdoutRl = readline.createInterface({ input: this.#proc.stdout });
      stdoutRl.on("line", (line) => logger.info(line));
    }

    if (this.#proc.stderr) {
      const stderrRl = readline.createInterface({ input: this.#proc.stderr });
      stderrRl.on("line", (line) => logger.error(line));
    }

    return this;
  }

  async collect(): Promise<ProcessResult> {
    const [stdout, stderr, exitCode] = await Promise.all([
      this.#proc.stdout ? text(this.#proc.stdout) : Promise.resolve(""),
      this.#proc.stderr ? text(this.#proc.stderr) : Promise.resolve(""),
      this.#closePromise,
    ]);
    return { exitCode, stdout, stderr };
  }

  async wait(): Promise<{ exitCode: number | null }> {
    const exitCode = await this.#closePromise;
    if (exitCode !== null && exitCode !== 0) {
      const logger = this.#logger ?? createLogger("process");
      throw new CLIError(logger, `Process exited with code ${exitCode}`);
    }
    return { exitCode };
  }

  async kill(): Promise<void> {
    if (!this.#proc.killed) {
      this.#proc.kill();
    }
    try {
      await this.#closePromise;
    } catch {
      // ignore errors, since we know we're killing the process anyway
    }
  }

  disposable(): AsyncDisposable {
    return {
      [Symbol.asyncDispose]: () => this.kill(),
    };
  }
}
