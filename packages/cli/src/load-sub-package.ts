// Finds the sub-package that holds the Wasp binary for the platform this
// process runs on.

import packageJson from "#/package.json" with { type: "json" };
import type {
  CliPackageData,
  CliSubPackageAPI,
  PlatformTarget,
} from "wasp-cli-package-data-types";
import { CLIError } from "./cli-error.js";
import { debug } from "./debug.js";
import { assertNodeVersionIsSupported } from "./node-version.js";
import {
  findSubPackageInfo,
  getCurrentPlatformTarget,
  UNDEFINED_LIBC_NAME,
} from "./package-data.js";

export async function loadSubPackageForCurrentPlatform(): Promise<CliSubPackageAPI> {
  const packageData = await readPackageData();
  const currentPlatform = getCurrentPlatformTarget();

  const subPackageInfo = findSubPackageInfo(packageData, currentPlatform);
  if (!subPackageInfo) {
    throw new CLIError(
      `Wasp is not supported on this platform (${formatPlatform(currentPlatform)}). ` +
        `Supported platforms: ${listSupportedPlatforms(packageData).join(", ")}.`,
    );
  }
  debug("Selected sub-package: %j", subPackageInfo);

  try {
    const subPackageModule: { default: CliSubPackageAPI } = await import(
      subPackageInfo.packageName
    );
    return subPackageModule.default;
  } catch (error: unknown) {
    // A common case for failure here is that the Node.js version is not
    // supported. `npm` doesn't like to complain when being specifically asked
    // to install a package, but will then **not** install our subpackages
    // because they are marked as "optionalDependencies". Because it is quite
    // a common case, we'll check if we're on an unsupported Node version and
    // throw a specific error for that.
    assertNodeVersionIsSupported(
      process.versions.node,
      packageJson.engines.node,
    );
    throw new CLIError(
      "Can't locate the correct executable for your platform. Try running `npm install` again.",
      { cause: error },
    );
  }
}

async function readPackageData(): Promise<CliPackageData> {
  // Only the published package has `data.json`, so TypeScript must see a
  // `string` instead of the path.
  const { default: packageData } = await import("#/data.json" as string, {
    with: { type: "json" },
  });
  return packageData as CliPackageData;
}

function listSupportedPlatforms(packageData: CliPackageData): string[] {
  return Object.entries(packageData.subPackages).flatMap(([os, cpus]) =>
    Object.entries(cpus).flatMap(([cpu, libcs]) =>
      Object.keys(libcs).map((libc) => formatPlatform({ os, cpu, libc })),
    ),
  );
}

// E.g. `linux-arm64-musl`, or `darwin-arm64` where libc doesn't apply.
function formatPlatform({ os, cpu, libc }: PlatformTarget): string {
  return [os, cpu, ...(libc === UNDEFINED_LIBC_NAME ? [] : [libc])].join("-");
}
