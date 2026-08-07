import { join } from "node:path";
import { getPackageJson, getWaspcDirPath } from "../utils.ts";

export function getDataPackagesDirPath(): string {
  const waspcDirPath = getWaspcDirPath();
  return join(waspcDirPath, "data", "packages");
}

export function orderPackageDirs(packageDirs: string[]): string[] {
  // Other packages import @wasp.sh/spec from its built dist, so spec builds first.
  const specDirs = packageDirs.filter(
    (dir) => getPackageJson(dir).name === "@wasp.sh/spec",
  );
  const otherDirs = packageDirs
    .filter((dir) => !specDirs.includes(dir))
    .sort((a, b) => a.localeCompare(b));

  return [...specDirs, ...otherDirs];
}
