// Checks that this Node.js is one that `@wasp/cli` supports.

import { CLIError } from "./cli-error.js";
import { debug } from "./debug.js";

type Version = readonly [major: number, minor: number, patch: number];

/**
 * Throws if the Node.js version is older than the one the range asks for.
 *
 * @param nodeVersion E.g. `process.versions.node`.
 * @param requiredNodeVersionRange E.g. `engines.node` of a `package.json`.
 */
export function assertNodeVersionIsSupported(
  nodeVersion: string,
  requiredNodeVersionRange: string,
): void {
  const minVersion = findVersionIn(requiredNodeVersionRange);
  const currentVersion = findVersionIn(nodeVersion);
  if (minVersion === undefined || currentVersion === undefined) {
    debug(
      "Can't compare Node.js %s with the required %s, skipping the check.",
      nodeVersion,
      requiredNodeVersionRange,
    );
    return;
  }

  if (!isVersionAtLeast(currentVersion, minVersion)) {
    throw new CLIError(
      `Your Node.js version (${currentVersion.join(".")}) is not supported. Please upgrade to Node.js ${minVersion.join(".")} or later to use Wasp.`,
    );
  }
}

/** The first `major.minor.patch` in the text, e.g. `26.13.1` in `>=26.13.1`. */
function findVersionIn(text: string): Version | undefined {
  const match = text.match(/(\d+)\.(\d+)\.(\d+)/);
  if (!match) {
    return undefined;
  }
  const [, major, minor, patch] = match;
  return [Number(major), Number(minor), Number(patch)];
}

function isVersionAtLeast(version: Version, minVersion: Version): boolean {
  for (const [index, part] of version.entries()) {
    if (part !== minVersion[index]) {
      return part > minVersion[index];
    }
  }
  return true;
}
