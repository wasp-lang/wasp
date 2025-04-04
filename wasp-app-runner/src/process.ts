import readline from "readline";
import { ChildProcess, spawn } from "child_process";
import { log } from "./logging.js";
import type { EnvVars } from "./types.js";

type SpawnOptions = {
  name: string;
  cmd: string;
  args: string[];
  cwd?: string;
};

class ChildProcessManager {
  private children: ChildProcess[] = [];

  constructor() {
    process.on("SIGINT", () => this.cleanExit("SIGINT"));
    process.on("SIGTERM", () => this.cleanExit("SIGTERM"));
    process.on("exit", () => this.cleanExit("exit"));
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

  private cleanExit(reason: string) {
    log("shutdown", "warn", `Received ${reason}. Cleaning up...`);
    this.children.forEach((child) => {
      if (!child.killed) {
        child.kill();
      }
    });
    process.exit();
  }
}

const childProcessManager = new ChildProcessManager();

export function spawnWithLog({
  name,
  cmd,
  args,
  cwd,
  extraEnv = {},
}: SpawnOptions & {
  extraEnv?: EnvVars;
}): Promise<{ exitCode: number | null }> {
  return new Promise((resolve, reject) => {
    const proc = spawn(cmd, args, {
      cwd,
      env: { ...process.env, ...extraEnv },
      stdio: ["ignore", "pipe", "pipe"],
    });
    childProcessManager.addChild(proc);

    readStreamLines(proc.stdout, (line) => log(name, "info", line));
    readStreamLines(proc.stderr, (line) => log(name, "error", line));

    proc.on("error", (err) => {
      log(name, "error", `Process error: ${err.message}`);
      reject(err);
    });

    proc.on("close", (exitCode) => {
      childProcessManager.removeChild(proc);
      if (exitCode === 0) {
        log(name, "success", "Process completed successfully");
        resolve({
          exitCode,
        });
      } else {
        log(name, "error", `Process exited with code ${exitCode}`);
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
}: SpawnOptions): Promise<{
  exitCode: number | null;
  stdoutData: string;
  stderrData: string;
}> {
  let stdoutData = "";
  let stderrData = "";
  return new Promise((resolve) => {
    const proc = spawn(cmd, args, {
      cwd,
    });
    childProcessManager.addChild(proc);

    readStreamLines(proc.stdout, (line) => (stdoutData += line + "\n"));
    readStreamLines(proc.stderr, (line) => (stderrData += line + "\n"));

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
  callback: (line: string) => void
) {
  const rl = readline.createInterface({
    input: stream,
    crlfDelay: Infinity,
  });

  rl.on("line", callback);
}
