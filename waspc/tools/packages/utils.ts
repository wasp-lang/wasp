import { join } from "node:path";
import { getRepoRootPath } from "../utils.ts";

// The dirs of the packages we publish to npm at the Wasp version. They are the
// CLI wrapper, the `@wasp/wasp` project dependency and the packages it brings.
export const WASP_PACKAGE_DIRS = ["cli", "deploy", "spec", "studio", "wasp"];

export function getPackageDirPath(packageName: string): string {
  return join(getRepoRootPath(), "packages", packageName);
}
