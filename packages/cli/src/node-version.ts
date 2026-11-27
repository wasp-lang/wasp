// Checks that this Node.js is one that `@wasp/cli` supports.

import satisfies from "semver/functions/satisfies.js";
import minVersion from "semver/ranges/min-version.js";
import { CLIError } from "./cli-error.js";

/**
 * Throws if the Node.js version is outside of the range.
 *
 * @param nodeVersion E.g. `process.versions.node`.
 * @param requiredNodeVersionRange E.g. `engines.node` of a `package.json`.
 */
export function assertNodeVersionIsSupported(
  nodeVersion: string,
  requiredNodeVersionRange: string,
): void {
  if (!satisfies(nodeVersion, requiredNodeVersionRange)) {
    throw new CLIError(
      `Your Node.js version (${nodeVersion}) is not supported. Please upgrade to Node.js ${minVersion(requiredNodeVersionRange)} or later to use Wasp.`,
    );
  }
}
