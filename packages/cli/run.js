// @ts-check

// Runs the Wasp binary of this `@wasp/cli`. Other Wasp CLI versions call
// `runWaspCli` when they hand over to this one (see `project-wasp-cli.js`), so
// keep its name, its signature and the `./run` export in package.json.

import { execFileSync } from "node:child_process";
import { report as processReport } from "node:process";
import { debuglog } from "node:util";
import { CLIError } from "./CLIError.js";

const debug = debuglog("wasp-bin-wrapper");

/**
 * Runs this CLI's Wasp binary with the process's arguments, and returns its
 * exit code.
 *
 * @returns {Promise<number>}
 */
export async function runWaspCli() {
  const packageJson = await getPackageJson();
  const { waspBinPath, dataDirPath } = await getSubPackage(packageJson).catch(
    CLIError.rethrowWith("Can't find the Wasp executable."),
  );

  return await runWasp({
    waspBinPath,
    dataDirPath,
    waspVersion: packageJson.version,
  }).catch(CLIError.rethrowWith("Failed to run Wasp."));
}

/**
 * @returns {Promise<string>}
 */
export async function getWaspCliVersion() {
  return (await getPackageJson()).version;
}

async function getSubPackage(/** @type {PackageJson} */ packageJson) {
  debug("Loading CLI package data...");

  /** @type {{ default: import("../../scripts/make-cli-npm-packages/src/schema/output-data.ts").CliPackageData }} */
  const { default: data } = await import(
    // @ts-expect-error This file does not exist at dev time, only in the built package.
    "./data.json",
    { with: { type: "json" } }
  );

  debug("Loaded CLI package data: %j", data);

  const { platform, arch } = process;
  const libc =
    platform === "linux"
      ? isGlibc()
        ? "glibc"
        : "musl"
      : /** @satisfies {typeof import("../../scripts/make-cli-npm-packages/src/schema/output-data.ts").UNDEFINED_LIBC_NAME} */ (
          "unknown"
        );

  debug("Selecting sub-package for %s / %s / %s", platform, arch, libc);

  const selectedSubPackage = data.subPackages[platform]?.[arch]?.[libc];
  if (!selectedSubPackage) {
    throw new CLIError("Wasp is not supported on this platform.");
  }

  debug("Selected sub-package: %j", selectedSubPackage);

  /** @type {{ default: import("../../scripts/make-cli-npm-packages/src/schema/output-data.ts").SubPackageAPI }} */
  const importedPackage = await import(selectedSubPackage.packageName)
    .catch(async (error) => {
      // A common case for failure here is that the Node.js version is not
      // supported. `npm` doesn't like to complain when being specifically asked
      // to install a package, but will then **not** install our subpackages
      // because they are marked as "optionalDependencies". Because it is quite
      // a common case, we'll check if we're on an unsupported Node version and
      // throw a specific error for that.
      assertNodeIsSupported(packageJson);
      throw error;
    })
    .catch(
      CLIError.rethrowWith(
        "Can't locate the correct executable for your platform. Try running `npm install` again.",
      ),
    );

  debug("Imported sub-package: %j", importedPackage);

  return importedPackage.default;
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

function assertNodeIsSupported(/** @type {PackageJson} */ packageJson) {
  const minVersion = getMinimumNodeVersion(packageJson);
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

function getMinimumNodeVersion(/** @type {PackageJson} */ packageJson) {
  const requirement = packageJson.engines.node;
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

async function runWasp(
  /** @type {import("../../scripts/make-cli-npm-packages/src/schema/output-data.ts").SubPackageAPI & { waspVersion: string }} */ {
    waspBinPath,
    dataDirPath,
    waspVersion,
  },
) {
  try {
    debug("Running Wasp executable at path: %s", waspBinPath);
    debug("Using data directory at path: %s", dataDirPath);
    debug("Passing arguments: %j", process.argv.slice(2));

    execFileSync(waspBinPath, process.argv.slice(2), {
      env: {
        ...process.env,
        waspc_datadir: dataDirPath,
        // The binary's own version comes from Cabal, which has no prerelease
        // versions like `0.27.0-rc.1`. The npm package version is the source
        // of truth for the Wasp versions the binary writes into package.json.
        WASP_CLI_NPM_PACKAGE_VERSION: waspVersion,
      },
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

/**
 * @typedef {{ version: string, engines: { node: string } }} PackageJson
 */

async function getPackageJson() {
  /** @type {{ default: PackageJson }} */
  const { default: packageJson } = await import("./package.json", {
    with: { type: "json" },
  });
  return packageJson;
}
