import { join } from "node:path";
import { getRepoRootPath, getWaspcDirPath } from "../utils.ts";

export function getLibsDirPath(): string {
  return join(getRepoRootPath(), "packages", "libs");
}

export function getDataLibsDirPath(): string {
  return join(getWaspcDirPath(), "data", "Generator", "libs");
}
