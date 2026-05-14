import { join } from "node:path";
import { getWaspcDirPath } from "../utils.ts";

export function getDataPackagesDirPath(): string {
  const waspcDirPath = getWaspcDirPath();
  return join(waspcDirPath, "data", "packages");
}
