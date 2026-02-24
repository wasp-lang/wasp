import { ChildProcess, spawn } from "child_process";
import readline from "readline";
import { createLogger } from "./logging.js";
import type { EnvVars } from "./types.js";

type SpawnOptions = {
  name: string;
  cmd: string;
  args: string[];
  cwd?: string;
};

class ChildProcessManager {
  private children: ChildProcess[] = [];
  private logger = createLogger("child-process-manager");

  connectSignal(signal: AbortSignal): void {
    signal.addEventListener("abort", () => {
      this.killAll("abort signal");
    });
  }

  addChild(child: ChildProcess) {
    this.children.push(child);
  }

  removeChild(proc: ChildProcess) {
    const index = this.children.indexOf(proc);
    if (index !== -1) {
      this.children.splice(index, 1);
    }
  }

  killAll(reason: string) {
    if (this.children.length === 0) {
      return;
    }
    this.logger.warn(`Received ${reason}. Cleaning up...`);
    for (const child of this.children) {
      if (!child.killed) {
        child.kill();
      }
    }
    this.children = [];
  }
}

export const childProcessManager = new ChildProcessManager();

export function spawnWithLog({
  name,
  cmd,
  args,
  cwd,
  extraEnv = {},
  signal,
}: SpawnOptions & {
  extraEnv?: EnvVars;
  signal?: AbortSignal;
}): Promise<{ exitCode: number | null }> {
  return new Promise((resolve, reject) => {
    const logger = createLogger(name);
    const proc = spawn(cmd, args, {
      cwd,
      env: { ...process.env, ...extraEnv },
      stdio: ["ignore", "pipe", "pipe"],
      signal,
    });
    childProcessManager.addChild(proc);

    readStreamLines(proc.stdout, (line) => logger.info(line));
    readStreamLines(proc.stderr, (line) => logger.error(line));

    proc.on("error", (err) => {
      if (err.name === "AbortError") {
        reject(err);
        return;
      }
      logger.error(`Process error: ${err.message}`);
      reject(err);
    });

    proc.on("close", (exitCode) => {
      childProcessManager.removeChild(proc);
      if (exitCode === 0) {
        logger.success("Process completed successfully");
        resolve({
          exitCode,
        });
      } else {
        logger.error(`Process exited with code ${exitCode}`);
        reject({
          exitCode,
        });
      }
    });
  });
}

export function spawnAndCollectOutput({
  cmd,
  args,
  cwd,
  signal,
}: SpawnOptions & {
  signal?: AbortSignal;
}): Promise<{
  exitCode: number | null;
  stdoutData: string;
  stderrData: string;
}> {
  let stdoutData = "";
  let stderrData = "";
  return new Promise((resolve, reject) => {
    const proc = spawn(cmd, args, {
      cwd,
      signal,
    });
    childProcessManager.addChild(proc);

    readStreamLines(proc.stdout, (line) => (stdoutData += line + "\n"));
    readStreamLines(proc.stderr, (line) => (stderrData += line + "\n"));

    proc.on("error", (err) => {
      childProcessManager.removeChild(proc);
      reject(err);
    });

    proc.on("close", (exitCode) => {
      childProcessManager.removeChild(proc);
      resolve({
        exitCode,
        stdoutData,
        stderrData,
      });
    });
  });
}

function readStreamLines(
  stream: NodeJS.ReadableStream,
  callback: (line: string) => void,
) {
  const rl = readline.createInterface({
    input: stream,
    crlfDelay: Infinity,
  });

  rl.on("line", callback);
}
