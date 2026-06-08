#!/usr/bin/env node

// @ts-check

import { execFileSync } from "node:child_process";
import { report as processReport } from "node:process";
import { debuglog } from "node:util";
import { CLIError } from "./CLIError.js";

const debug = debuglog("wasp-bin-wrapper");

try {
  const childProcessExitCode = await main();
  debug("Wasp process exited with code: %d", childProcessExitCode);
  process.exitCode = childProcessExitCode;
} catch (error) {
  debug("Error in launching Wasp: %o", error);
  // An error happened while trying to find or run the Wasp executable.
  // Report it and exit with an unknown failure error code.
  // Errors inside the Wasp process itself are not in this wrapper, as they are
  // handled by mimicking its exit code above.
  CLIError.log(error);
  process.exitCode = -1;
} finally {
  debug("Exiting Wasp wrapper process with code: %d", process.exitCode);
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
  debug("Loading main package data...");

  /** @type {{ default: import("../../src/schema/output-data.ts").MainPackageData }} */
  const { default: data } = await import(
    // @ts-expect-error This file does not exist at dev time, only in the built package.
    "./data.json",
    { with: { type: "json" } }
  );

  debug("Loaded main package data: %j", data);

  const { platform, arch } = process;
  const libc =
    platform === "linux"
      ? isGlibc()
        ? "glibc"
        : "musl"
      : /** @satisfies {typeof import("../../src/schema/output-data.ts").UNDEFINED_LIBC_NAME} */ (
          "unknown"
        );

  debug("Selecting sub-package for %s / %s / %s", platform, arch, libc);

  const selectedSubPackage = data.subPackages[platform]?.[arch]?.[libc];
  if (!selectedSubPackage) {
    throw new CLIError("Wasp is not supported on this platform.");
  }

  debug("Selected sub-package: %j", selectedSubPackage);

  /** @type {{ default: import("../../src/schema/output-data.ts").SubPackageAPI }} */
  const importedPackage = await import(selectedSubPackage.packageName)
    .catch(async (error) => {
      // A common case for failure here is that the Node.js version is not
      // supported. `npm` doesn't like to complain when being specifically asked
      // to install a package, but will then **not** install our subpackages
      // because they are marked as "optionalDependencies". Because it is quite
      // a common case, we'll check if we're on an unsupported Node version and
      // throw a specific error for that.
      await assertNodeIsSupported();
      throw error;
    })
    .catch(
      CLIError.rethrowWith(
        "Can't locate the correct executable for your platform.",
      ),
    );

  debug("Imported sub-package: %j", importedPackage);

  return importedPackage.default;
}

async function runWasp(
  /** @type {import("../../src/schema/output-data.ts").SubPackageAPI} */ {
    waspBinPath,
    dataDirPath,
  },
) {
  try {
    debug("Running Wasp executable at path: %s", waspBinPath);
    debug("Using data directory at path: %s", dataDirPath);
    debug("Passing arguments: %j", process.argv.slice(2));

    execFileSync(waspBinPath, process.argv.slice(2), {
      env: { ...process.env, waspc_datadir: dataDirPath },
      stdio: "inherit",
    });

    debug("Wasp executed successfully.");

    return 0;
  } catch (/** @type {any} */ e) {
    debug("Wasp execution failed with error: %o", e);
    // We do a loose equality check because the documentation points to this property
    // being `number | undefined`, but from testing it seems it can also be `null`.
    if (e.status != undefined) {
      // The executed process ran correctly, but exited with a non-zero status
      // code. This is not considered an error in the scope of this wrapper,
      // so we just return the exit code.
      debug("We got an exit code from Wasp: %d", e.status);
      return /** @type {number} */ (e.status);
    } else {
      throw e;
    }
  }
}

async function assertNodeIsSupported() {
  const minVersion = await getMinimumNodeVersion();
  if (!minVersion) {
    debug(
      "No minimum Node.js version specified in package.json, skipping check.",
    );
    return;
  }

  const currentVersion = getCurrentNodeVersion();

  debug(
    "Minimum required Node.js version: %j, current: %j",
    minVersion,
    currentVersion,
  );

  const nodeVersionIsSupported = checkNodeVersionIsSupported({
    minVersion,
    currentVersion,
  });

  if (!nodeVersionIsSupported) {
    throw new CLIError(
      `Your Node.js version (${currentVersion.join(".")}) is not supported. Please upgrade to Node.js ${minVersion.join(".")} to use Wasp.`,
    );
  }
}

async function getMinimumNodeVersion() {
  // We import here and not in the top level to keep the happy path free of this
  // overhead.
  const { default: pkg } = await import("./package.json", {
    with: { type: "json" },
  });

  const requirement = pkg.engines.node;
  const requirementMatch = requirement.match(/(\d+)\.(\d+)\.(\d+)/);
  if (!requirementMatch) return undefined;

  const [_, major, minor, patch] = requirementMatch;
  return [
    Number.parseInt(major),
    Number.parseInt(minor),
    Number.parseInt(patch),
  ];
}

function getCurrentNodeVersion() {
  const [major, minor, patch] = process.versions.node.split(".");
  return [
    Number.parseInt(major),
    Number.parseInt(minor),
    Number.parseInt(patch),
  ];
}

function checkNodeVersionIsSupported(
  /** @type {{minVersion: number[], currentVersion: number[]}} */
  { minVersion, currentVersion },
) {
  for (let i = 0; i < minVersion.length; i++) {
    const currentPart = currentVersion[i];
    const requiredPart = minVersion[i];
    if (currentPart > requiredPart) return true;
    if (currentPart === requiredPart) continue;
    if (currentPart < requiredPart) return false;
  }

  // If we reach the end it's because all parts are equal, so the current
  // version satisfies the requirement.
  return true;
}

// Adapted from rollup
// https://github.com/rollup/rollup/blob/c5f3e1d3162ccb36e18704138c21457a084ef358/native.js#L5-L11
function isGlibc() {
  try {
    const report = /** @type {any} */ (processReport.getReport()).header;
    debug("Process report header: %j", report);
    const hasGlibc = Boolean(report.glibcVersionRuntime);
    debug("Has glibc: %s", hasGlibc);
    return hasGlibc;
  } catch {
    debug("Failed to get process report, assuming not glibc.");
    return false;
  }
}
