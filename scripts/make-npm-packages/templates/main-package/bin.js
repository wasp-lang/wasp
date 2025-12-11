#!/usr/bin/env node

// @ts-check

import { execFileSync } from "node:child_process";
import { report as processReport } from "node:process";
import { CLIError } from "./CLIError.js";

try {
  const childProcessExitCode = await main();
  process.exitCode = childProcessExitCode;
} catch (error) {
  // An error happened while trying to find or run the Wasp executable.
  // Report it and exit with an unknown failure error code.
  // Errors inside the Wasp process itself are not in this wrapper, as they are
  // handled by mimicking its exit code above.
  CLIError.log(error);
  process.exitCode = -1;
}

async function main() {
  const { waspBinPath, dataDirPath } = await getSubPackage().catch(
    CLIError.rethrowWith("Can't find the Wasp executable."),
  );

  return await runWasp({ waspBinPath, dataDirPath }).catch(
    CLIError.rethrowWith("Failed to run Wasp."),
  );
}

async function getSubPackage() {
  /** @type {{ default: import("../../src/schema/output-data.ts").MainPackageData }} */
  const { default: data } = await import(
    // @ts-expect-error This file does not exist at dev time, only in the built package.
    "./data.json",
    { with: { type: "json" } }
  );

  const selectedSubPackage =
    data.subPackages[process.platform]?.[process.arch]?.[
      isMusl() ? "musl" : "unknown"
    ];
  if (!selectedSubPackage) {
    throw new CLIError("Wasp is not supported on this platform.");
  }

  /** @type {{ default: import("../../src/schema/output-data.ts").SubPackageAPI }} */
  const importedPackage = await import(selectedSubPackage.packageName).catch(
    CLIError.rethrowWith(
      "Can't locate the correct executable for your platform.",
    ),
  );

  return importedPackage.default;
}

async function runWasp(
  /** @type {import("../../src/schema/output-data.ts").SubPackageAPI} */ {
    waspBinPath,
    dataDirPath,
  },
) {
  try {
    execFileSync(waspBinPath, process.argv.slice(2), {
      env: { ...process.env, waspc_datadir: dataDirPath },
      stdio: "inherit",
    });
    return 0;
  } catch (/** @type {any} */ e) {
    if (e.status !== undefined) {
      // The executed process ran correctly, but exited with a non-zero status
      // code. This is not considered an error in the scope of this wrapper,
      // so we just return the exit code.
      return e.status;
    } else {
      throw e;
    }
  }
}

// Copied from rollup
// https://github.com/rollup/rollup/blob/c5f3e1d3162ccb36e18704138c21457a084ef358/native.js#L5-L11
function isMusl() {
  try {
    return !(
      /** @type {any} */ (processReport.getReport()).header.glibcVersionRuntime
    );
  } catch {
    return false;
  }
}
