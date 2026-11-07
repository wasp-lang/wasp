// Runs the Wasp binary of this `@wasp/cli`.

import { execFileSync } from "node:child_process";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { debuglog } from "node:util";
import { CLIError } from "./cli-error.js";
import {
  getCurrentPlatformTarget,
  type CliPackageData,
  type CliSubPackageAPI,
} from "./package-data.js";

const debug = debuglog("wasp-bin-wrapper");

interface PackageJson {
  version: string;
  engines: { node: string };
}

/** Returns the Wasp binary's exit code. */
export async function runWaspCli(): Promise<number> {
  const packageJson = await readPackageJson();
  const { waspBinPath, dataDirPath } = await getSubPackage(packageJson).catch(
    CLIError.rethrowWith("Can't find the Wasp executable."),
  );

  return await runWasp({
    waspBinPath,
    dataDirPath,
    waspVersion: packageJson.version,
  }).catch(CLIError.rethrowWith("Failed to run Wasp."));
}

async function getSubPackage(
  packageJson: PackageJson,
): Promise<CliSubPackageAPI> {
  debug("Loading CLI package data...");

  const data = await readPackageData();

  debug("Loaded CLI package data: %j", data);

  const { os, cpu, libc } = getCurrentPlatformTarget();

  debug("Selecting sub-package for %s / %s / %s", os, cpu, libc);

  const selectedSubPackage = data.subPackages[os]?.[cpu]?.[libc];
  if (!selectedSubPackage) {
    throw new CLIError("Wasp is not supported on this platform.");
  }

  debug("Selected sub-package: %j", selectedSubPackage);

  const importedPackage: { default: CliSubPackageAPI } = await import(
    selectedSubPackage.packageName
  )
    .catch(async (error: unknown) => {
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

function assertNodeIsSupported(packageJson: PackageJson): void {
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

function getMinimumNodeVersion(packageJson: PackageJson): number[] | undefined {
  const requirement = packageJson.engines.node;
  const requirementMatch = requirement.match(/(\d+)\.(\d+)\.(\d+)/);
  if (!requirementMatch) return undefined;

  const [, major, minor, patch] = requirementMatch;
  return [
    Number.parseInt(major),
    Number.parseInt(minor),
    Number.parseInt(patch),
  ];
}

function getCurrentNodeVersion(): number[] {
  const [major, minor, patch] = process.versions.node.split(".");
  return [
    Number.parseInt(major),
    Number.parseInt(minor),
    Number.parseInt(patch),
  ];
}

function checkNodeVersionIsSupported({
  minVersion,
  currentVersion,
}: {
  minVersion: number[];
  currentVersion: number[];
}): boolean {
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

async function runWasp({
  waspBinPath,
  dataDirPath,
  waspVersion,
}: CliSubPackageAPI & { waspVersion: string }): Promise<number> {
  try {
    debug("Running Wasp executable at path: %s", waspBinPath);
    debug("Using data directory at path: %s", dataDirPath);
    debug("Passing arguments: %j", process.argv.slice(2));

    execFileSync(waspBinPath, process.argv.slice(2), {
      env: {
        ...process.env,
        waspc_datadir: dataDirPath,
        // Cabal versions can't express npm prereleases like `0.27.0-rc.1`, so
        // the binary takes its version from here.
        WASP_CLI_NPM_PACKAGE_VERSION: waspVersion,
      },
      stdio: "inherit",
    });

    debug("Wasp executed successfully.");

    return 0;
  } catch (error: unknown) {
    debug("Wasp execution failed with error: %o", error);
    const status = (error as { status?: number | null }).status;
    // We do a loose equality check because the documentation points to this property
    // being `number | undefined`, but from testing it seems it can also be `null`.
    if (status != undefined) {
      // The executed process ran correctly, but exited with a non-zero status
      // code. This is not considered an error in the scope of this wrapper,
      // so we just return the exit code.
      debug("We got an exit code from Wasp: %d", status);
      return status;
    } else {
      throw error;
    }
  }
}

async function readPackageJson(): Promise<PackageJson> {
  return readJsonFromPackageRoot("package.json");
}

// `data.json` doesn't exist at dev time, only in the built package.
async function readPackageData(): Promise<CliPackageData> {
  return readJsonFromPackageRoot("data.json");
}

// The compiled code runs from `dist/`, one level below the package root.
async function readJsonFromPackageRoot<T>(fileName: string): Promise<T> {
  const filePath = path.join(import.meta.dirname, "..", fileName);
  return JSON.parse(await fs.readFile(filePath, "utf8")) as T;
}
