import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath } from "../utils.ts";

// The TypeScript packages `waspc` ships in its data dir. Their sources live in
// `<repo>/packages/`, and the build copies their artifacts into the Cabal data
// dir, where `waspc` expects them.
export const WASPC_PACKAGE_NAMES = ["spec"];

// Packages published to npm at the Wasp version but not shipped in the data
// dir: the CLI wrapper, the `@wasp/wasp` project dependency and the packages
// it runs from the project (`wasp deploy`, `wasp studio`).
export const PUBLISHED_WASP_PACKAGE_NAMES = ["cli", "deploy", "studio", "wasp"];

export function getPackageDirPath(packageName: string): string {
  return join(getRepoRootPath(), "packages", packageName);
}

export function getDataPackagesDirPath(): string {
  return join(getWaspcDirPath(), "data", "packages");
}
