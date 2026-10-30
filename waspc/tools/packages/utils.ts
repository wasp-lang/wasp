import { join } from "node:path";
import { getRepoRootPath } from "../utils.ts";

// The packages published to npm at the Wasp version: the CLI wrapper, the
// `@wasp/wasp` project dependency and the packages it brings along.
export const WASP_PACKAGE_NAMES = ["cli", "deploy", "spec", "studio", "wasp"];

export function getPackageDirPath(packageName: string): string {
  return join(getRepoRootPath(), "packages", packageName);
}
