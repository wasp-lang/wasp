import { spawn } from "node:child_process";
import readline from "node:readline";
import { setTimeout as delay } from "node:timers/promises";
import { registerExitGuard } from "./exitGuard.js";
import { createLogger } from "./logging.js";
import type { EnvVars } from "./types.js";

const DEFAULT_KILL_GRACE_PERIOD_MS = 1000;

// Rolling cap on the per-stream output we keep in memory. Enough to surface a
// failure hint without growing unbounded for long-running, chatty processes.
const MAX_COLLECTED_OUTPUT = 64 * 1024;

export type ProcessExit =
  | { kind: "exited"; exitCode: number }
  | { kind: "terminated"; signal: NodeJS.Signals }
  | { kind: "spawn-failed"; error: Error };

export type OutputMode = "log" | "collect" | "ignore";

export type SpawnProcessOptions = {
  name: string;
  cmd: string;
  args: string[];
  cwd?: string;
  extraEnv?: EnvVars;
  /** Aborting this signal terminates the process (SIGTERM, then SIGKILL). */
  signal?: AbortSignal;
  /** "log" (default) prints each line; "collect" only buffers; "ignore" drains silently. */
  output?: OutputMode;
  /** How long to wait after SIGTERM before SIGKILL. Default 1000ms. */
  killGracePeriodMs?: number;
  /** Run in its own process group so the whole tree can be killed. Default false. */
  detached?: boolean;
};

export interface ProcessHandle extends AsyncDisposable {
  readonly name: string;
  /** Resolves once, never rejects. */
  readonly exited: Promise<ProcessExit>;
  /** Rolling, capped view of what the process has written so far. */
  readonly collectedOutput: { stdout: string; stderr: string };
  /** Idempotent graceful-then-forceful termination. Also the async-dispose action. */
  terminate(): Promise<ProcessExit>;
}

export function spawnProcess(opts: SpawnProcessOptions): ProcessHandle {
  const {
    name,
    cmd,
    args,
    cwd,
    extraEnv = {},
    signal,
    output = "log",
    killGracePeriodMs = DEFAULT_KILL_GRACE_PERIOD_MS,
    detached = false,
  } = opts;

  const logger = output === "log" ? createLogger(name) : null;

  const child = spawn(cmd, args, {
    cwd,
    env: { ...process.env, ...extraEnv },
    stdio: ["ignore", "pipe", "pipe"],
    detached,
  });

  let stdoutBuf = "";
  let stderrBuf = "";

  consumeStream(child.stdout, (line) => {
    stdoutBuf = appendCapped(stdoutBuf, line);
    logger?.info(line);
  });
  consumeStream(child.stderr, (line) => {
    stderrBuf = appendCapped(stderrBuf, line);
    logger?.error(line);
  });

  // `exited` resolves exactly once and never rejects.
  let settled = false;
  let resolveExited!: (exit: ProcessExit) => void;
  const exited = new Promise<ProcessExit>((resolve) => {
    resolveExited = resolve;
  });
  function settle(exit: ProcessExit): void {
    if (settled) {
      return;
    }
    settled = true;
    resolveExited(exit);
  }

  // 'error' (e.g. ENOENT) and 'close' can both fire; `settle` keeps the first.
  child.on("error", (error) => {
    settle({ kind: "spawn-failed", error });
  });
  child.on("close", (code, closeSignal) => {
    if (closeSignal !== null) {
      settle({ kind: "terminated", signal: closeSignal });
    } else {
      settle({ kind: "exited", exitCode: code ?? 0 });
    }
  });

  function killWith(sig: NodeJS.Signals): void {
    if (settled || child.pid === undefined) {
      return;
    }
    try {
      if (detached) {
        // Negative pid targets the whole process group.
        process.kill(-child.pid, sig);
      } else {
        child.kill(sig);
      }
    } catch (error) {
      // ESRCH means it's already gone; anything else is best-effort cleanup.
      if ((error as NodeJS.ErrnoException).code !== "ESRCH") {
        logger?.warn(`Failed to send ${sig}: ${String(error)}`);
      }
    }
  }

  let terminatePromise: Promise<ProcessExit> | undefined;
  function terminate(): Promise<ProcessExit> {
    terminatePromise ??= doTerminate();
    return terminatePromise;
  }
  async function doTerminate(): Promise<ProcessExit> {
    if (settled) {
      return exited;
    }

    killWith("SIGTERM");

    const graceCtl = new AbortController();
    const winner = await Promise.race([
      exited.then(() => "exited" as const),
      delay(killGracePeriodMs, "timeout" as const, {
        signal: graceCtl.signal,
      }).catch(() => "exited" as const),
    ]);

    if (winner === "timeout") {
      killWith("SIGKILL");
    } else {
      // Cancel the still-pending grace timer so it can't hold the event loop.
      graceCtl.abort();
    }

    return exited;
  }

  // Synchronous safety net for exits that skip async teardown.
  const unregisterGuard = registerExitGuard(() => {
    if (settled || child.pid === undefined) {
      return;
    }
    try {
      process.kill(detached ? -child.pid : child.pid, "SIGKILL");
    } catch {
      // Already gone.
    }
  });

  const onAbort = () => void terminate();
  if (signal) {
    if (signal.aborted) {
      void terminate();
    } else {
      signal.addEventListener("abort", onAbort, { once: true });
    }
  }

  if (logger) {
    void exited.then((exit) => {
      if (isSuccessfulExit(exit)) {
        logger.success("Process completed successfully");
      } else {
        logger.warn(`Process ${describeExit(exit)}`);
      }
    });
  }

  void exited.finally(() => {
    unregisterGuard();
    signal?.removeEventListener("abort", onAbort);
  });

  return {
    name,
    exited,
    get collectedOutput() {
      return { stdout: stdoutBuf, stderr: stderrBuf };
    },
    terminate,
    async [Symbol.asyncDispose]() {
      await terminate();
    },
  };
}

export class CommandError extends Error {
  readonly commandName: string;
  readonly exit: ProcessExit;
  readonly stderr?: string;

  constructor(commandName: string, exit: ProcessExit, stderr?: string) {
    super(`Command "${commandName}" ${describeExit(exit)}`);
    this.name = "CommandError";
    this.commandName = commandName;
    this.exit = exit;
    this.stderr = stderr;
  }
}

type CommandOptions = Omit<
  SpawnProcessOptions,
  "output" | "detached" | "killGracePeriodMs"
>;

/** Runs a short command, streaming its output; throws {@link CommandError} unless it exits 0. */
export async function runCommand(opts: CommandOptions): Promise<void> {
  await using proc = spawnProcess({ ...opts, output: "log" });
  const exit = await proc.exited;
  if (!isSuccessfulExit(exit)) {
    throw new CommandError(opts.name, exit, proc.collectedOutput.stderr);
  }
}

/** Runs a short command, capturing its output; throws {@link CommandError} unless it exits 0. */
export async function captureCommand(
  opts: CommandOptions,
): Promise<{ stdout: string; stderr: string }> {
  await using proc = spawnProcess({ ...opts, output: "collect" });
  const exit = await proc.exited;
  const collected = proc.collectedOutput;
  if (!isSuccessfulExit(exit)) {
    throw new CommandError(opts.name, exit, collected.stderr);
  }
  return collected;
}

/**
 * Runs a command and reports whether it succeeded. Returns false on any
 * non-zero / spawn failure, but rethrows if the failure was caused by aborting
 * the run signal (so shutdown propagates instead of looking like a plain "no").
 */
export async function commandSucceeds(opts: CommandOptions): Promise<boolean> {
  await using proc = spawnProcess({ ...opts, output: "collect" });
  const exit = await proc.exited;
  if (opts.signal?.aborted) {
    throw opts.signal.reason;
  }
  return isSuccessfulExit(exit);
}

export function isSuccessfulExit(exit: ProcessExit): boolean {
  return exit.kind === "exited" && exit.exitCode === 0;
}

export function describeExit(exit: ProcessExit): string {
  switch (exit.kind) {
    case "exited":
      return `exited with code ${exit.exitCode}`;
    case "terminated":
      return `was terminated by ${exit.signal}`;
    case "spawn-failed":
      return `failed to start: ${exit.error.message}`;
  }
}

function appendCapped(buffer: string, line: string): string {
  const combined = buffer + line + "\n";
  return combined.length > MAX_COLLECTED_OUTPUT
    ? combined.slice(combined.length - MAX_COLLECTED_OUTPUT)
    : combined;
}

function consumeStream(
  stream: NodeJS.ReadableStream,
  onLine: (line: string) => void,
): void {
  const rl = readline.createInterface({ input: stream, crlfDelay: Infinity });
  rl.on("line", onLine);
}
