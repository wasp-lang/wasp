#!/usr/bin/env node

import { execFileSync } from "node:child_process";

async function main() {
  const { waspBinPath, dataDirPath } = await findWaspExecutable().catch(
    CLIError.rethrowWith("Can't find the Wasp executable."),
  );

  return await runWasp({ waspBinPath, dataDirPath }).catch(
    CLIError.rethrowWith("Failed to run Wasp."),
  );
}

async function findWaspExecutable() {
  const { default: data } = await import("./data.json", {
    with: { type: "json" },
  });

  const { waspBinPath, dataDirPath } = await Promise.any(
    data.subPackageNames.map((pkg) => import(pkg)),
  ).catch(
    CLIError.rethrowWith(
      "Can't locate the correct executable for your platform.",
    ),
  );

  return { waspBinPath, dataDirPath };
}

async function runWasp({ waspBinPath, dataDirPath }) {
  try {
    execFileSync(waspBinPath, process.argv.slice(2), {
      env: { ...process.env, waspc_datadir: dataDirPath },
      stdio: "inherit",
    });
    return 0;
  } catch (e) {
    if (e.status !== undefined) {
      // The executed process ran correctly, but exited with a non-zero status
      // code.
      return e.status;
    } else {
      throw e;
    }
  }
}

class CLIError extends Error {
  static rethrowWith = (message) => (cause) => {
    if (cause instanceof CLIError) {
      // This is already a CLIError with a proper message, so just rethrow it.
      throw cause;
    } else {
      // Wrap the original error into a new CLIError with the provided message.
      throw new CLIError(message, { cause });
    }
  };

  static log = (error) => {
    if (error instanceof CLIError) {
      console.error("Error:", error.message);
    } else {
      console.error(
        "Unknown Error launching Wasp:",
        error.message || String(error),
      );
    }

    if (error.cause) {
      console.error("Caused by:", error.cause.message || String(error.cause));
    }
  };
}

try {
  process.exitCode = await main();
} catch (error) {
  CLIError.log(error);
  process.exitCode = -1;
}
