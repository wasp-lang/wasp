import readline from "readline";
import { ChildProcess, spawn } from "child_process";
import { log } from "./logging.js";

export const processManager = setupProcessManager();

type SpawnOptions = {
  name: string;
  cmd: string;
  args: string[];
  cwd?: string;
};

function setupProcessManager() {
  const children: ChildProcess[] = [];

  function addChild(child: ChildProcess) {
    children.push(child);
  }

  function removeChild(proc: ChildProcess) {
    const index = children.indexOf(proc);
    if (index !== -1) {
      children.splice(index, 1);
    }
  }

  const cleanExit = (reason: string) => {
    log("shutdown", "warn", `Received ${reason}. Cleaning up...`);
    children.forEach((child) => {
      if (!child.killed) {
        child.kill();
      }
    });
    process.exit();
  };

  process.on("SIGINT", () => cleanExit("SIGINT"));
  process.on("SIGTERM", () => cleanExit("SIGTERM"));

  function spawnWithLog({
    name,
    cmd,
    args,
    cwd,
    extraEnv = {},
  }: SpawnOptions & {
    extraEnv?: Record<string, string>;
  }): Promise<number | null> {
    return new Promise((resolve, reject) => {
      const proc = spawn(cmd, args, {
        cwd,
        env: { ...process.env, ...extraEnv },
        stdio: ["ignore", "pipe", "pipe"],
      });
      addChild(proc);

      const handleStream = (
        stream: NodeJS.ReadableStream,
        type: "stdout" | "stderr"
      ) => {
        const rl = readline.createInterface({
          input: stream,
          crlfDelay: Infinity,
        });

        rl.on("line", (line) => {
          log(name, type === "stderr" ? "error" : "info", line);
        });
      };

      handleStream(proc.stdout, "stdout");
      handleStream(proc.stderr, "stderr");

      proc.on("error", (err) => {
        log(name, "error", `Process error: ${err.message}`);
        reject(err);
      });

      proc.on("close", (code) => {
        removeChild(proc);
        if (code === 0) {
          log(name, "success", "Process completed successfully");
          resolve(code);
        } else {
          log(name, "error", `Process exited with code ${code}`);
          reject(code);
        }
      });
    });
  }

  function spawnAndCollectStdout({ cmd, args, cwd }: SpawnOptions): Promise<{
    exitCode: number | null;
    stdoutData: string;
  }> {
    let stdoutData = "";
    return new Promise((resolve, reject) => {
      const proc = spawn(cmd, args, {
        cwd,
      });
      addChild(proc);

      proc.stdout.on("data", (data) => (stdoutData += data));
      proc.on("close", (exitCode) => {
        removeChild(proc);
        resolve({
          exitCode,
          stdoutData,
        });
      });
    });
  }

  return {
    spawnWithLog,
    spawnAndCollectStdout,
  };
}
