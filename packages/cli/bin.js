#!/usr/bin/env node

// @ts-check

import { execFileSync } from "node:child_process";
import * as fs from "node:fs";
import { createRequire } from "node:module";
import * as os from "node:os";
import * as path from "node:path";
import { report as processReport } from "node:process";
import { fileURLToPath } from "node:url";
import { debuglog } from "node:util";
import { CLIError } from "./CLIError.js";

// If you want to turn on debug logging, set the following environment variable:
// `NODE_DEBUG=wasp-bin-wrapper`
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
  assertNoLegacyInstallerConflict();

  const projectWaspCliBinPath = findProjectWaspCliBin();
  if (projectWaspCliBinPath !== undefined) {
    return runProjectWaspCli(projectWaspCliBinPath);
  }

  const { waspBinPath, dataDirPath } = await getSubPackage().catch(
    CLIError.rethrowWith("Can't find the Wasp executable."),
  );

  return await runWasp({ waspBinPath, dataDirPath }).catch(
    CLIError.rethrowWith("Failed to run Wasp."),
  );
}

// A project depends on Wasp through `@wasp/wasp`, which pins its own
// `@wasp/cli`. When a wrapper from elsewhere (typically a global install) runs
// inside such a project, it hands over to the project's wrapper, so the
// project always runs the Wasp version it declares.
function findProjectWaspCliBin() {
  let dir = process.cwd();
  while (true) {
    const waspPackageJsonPath = path.join(
      dir,
      "node_modules",
      "@wasp",
      "wasp",
      "package.json",
    );
    if (fs.existsSync(waspPackageJsonPath)) {
      return getProjectWaspCliBin(waspPackageJsonPath);
    }
    const parentDir = path.dirname(dir);
    if (parentDir === dir) {
      return undefined;
    }
    dir = parentDir;
  }
}

function getProjectWaspCliBin(/** @type {string} */ waspPackageJsonPath) {
  // Resolving from `@wasp/wasp` itself finds its `@wasp/cli` under any
  // package manager layout, as long as we start from the package's real
  // location. With pnpm, `node_modules/@wasp/wasp` is a symlink into the
  // virtual store, and `@wasp/cli` is only linked next to the real package.
  const require = createRequire(fs.realpathSync(waspPackageJsonPath));
  let projectWaspCliBinPath;
  try {
    projectWaspCliBinPath = fs.realpathSync(
      require.resolve("@wasp/cli/bin.js"),
    );
  } catch (error) {
    debug(
      "Found %s, but couldn't resolve its @wasp/cli: %o",
      waspPackageJsonPath,
      error,
    );
    return undefined;
  }

  const thisBinPath = fs.realpathSync(fileURLToPath(import.meta.url));
  if (projectWaspCliBinPath === thisBinPath) {
    debug("Running as the project's own Wasp CLI.");
    return undefined;
  }

  debug("Found the project's Wasp CLI at %s", projectWaspCliBinPath);
  return projectWaspCliBinPath;
}

function runProjectWaspCli(/** @type {string} */ projectWaspCliBinPath) {
  try {
    debug("Handing over to the project's Wasp CLI: %s", projectWaspCliBinPath);
    execFileSync(
      process.execPath,
      [projectWaspCliBinPath, ...process.argv.slice(2)],
      {
        stdio: "inherit",
      },
    );
    return 0;
  } catch (/** @type {any} */ e) {
    // Same as in `runWasp`, a non-zero exit of the project's Wasp is not an
    // error of this wrapper.
    if (e.status != undefined) {
      return /** @type {number} */ (e.status);
    } else {
      throw e;
    }
  }
}

async function getSubPackage() {
  debug("Loading main package data...");

  /** @type {{ default: import("../../scripts/make-npm-packages/src/schema/output-data.ts").MainPackageData }} */
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
      : /** @satisfies {typeof import("../../scripts/make-npm-packages/src/schema/output-data.ts").UNDEFINED_LIBC_NAME} */ (
          "unknown"
        );

  debug("Selecting sub-package for %s / %s / %s", platform, arch, libc);

  const selectedSubPackage = data.subPackages[platform]?.[arch]?.[libc];
  if (!selectedSubPackage) {
    throw new CLIError("Wasp is not supported on this platform.");
  }

  debug("Selected sub-package: %j", selectedSubPackage);

  /** @type {{ default: import("../../scripts/make-npm-packages/src/schema/output-data.ts").SubPackageAPI }} */
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
  /** @type {import("../../scripts/make-npm-packages/src/schema/output-data.ts").SubPackageAPI} */ {
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

// Wasp used to be installed with a shell installer script into
// `~/.local/share/wasp-lang`. Having both that and the npm package installed
// leads to confusing conflicts, so we refuse to run until the user migrates.
//
// This runs from `bin.js` instead of as an install script, because package
// managers increasingly block install scripts of dependencies (pnpm, bun). It
// does its work once per machine and then costs a single `stat` per invocation.
//
// TODO: Consider removing this check once we no longer expect any users to
// have a shell-installer based Wasp installation.
//
// These paths are also defined in:
// - https://github.com/wasp-lang/wasp/blob/main/waspc/cli/src/Wasp/Cli/FileSystem.hs
// - https://github.com/wasp-lang/get-wasp-sh/blob/master/installer.sh
// TODO: Do not hardcode: https://github.com/wasp-lang/wasp/issues/980
function getWaspLangDir() {
  return path.join(os.homedir(), ".local", "share", "wasp-lang");
}

function getNpmMarkerFile() {
  return path.join(getWaspLangDir(), ".uses-npm");
}

function assertNoLegacyInstallerConflict() {
  // No home dir (some containers), so no installer-based Wasp to conflict with.
  if (!hasHomeDir()) {
    return;
  }

  // Once the marker exists this is a single `stat`, so the directory listing
  // below only ever runs on the first invocation on a machine.
  if (hasNpmMarker()) {
    return;
  }

  if (hasInstallerWasp()) {
    throw new CLIError(`\
Detected an existing legacy Wasp installation.

You have Wasp installed via the curl installer script. To prevent conflicts,
please migrate to npm before using the npm package.

To migrate, run:

  curl -sSL https://get.wasp.sh/installer.sh | sh -s -- migrate-to-npm

and follow the instructions.`);
  }

  ensureNpmMarker();
}

function hasHomeDir() {
  try {
    os.homedir();
    return true;
  } catch {
    return false;
  }
}

function hasInstallerWasp() {
  try {
    // If the folder exists and has any subdirectories (for the installed version(s)),
    // we assume it's an installer-based Wasp installation.
    const entries = fs.readdirSync(getWaspLangDir(), { withFileTypes: true });
    return entries.some((entry) => entry.isDirectory());
  } catch {
    return false;
  }
}

function hasNpmMarker() {
  return fs.existsSync(getNpmMarkerFile());
}

// The installer script checks for this marker to warn about the same conflict
// from its side.
function ensureNpmMarker() {
  try {
    fs.mkdirSync(getWaspLangDir(), { recursive: true });

    // Same as `touch` - By appending nothing, we create the file if it doesn't
    // exist, or leave the contents unchanged if it does.
    fs.appendFileSync(getNpmMarkerFile(), "");
  } catch {
    // Ignore errors - marker file is best-effort
  }
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
