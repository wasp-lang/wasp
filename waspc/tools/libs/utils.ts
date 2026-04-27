import { join } from "node:path";
import { getWaspcDirPath } from "../utils.ts";

export function getDataLibsDirPath(): string {
  const waspcDirPath = getWaspcDirPath();
  return join(waspcDirPath, "data", "Generator", "libs");
}
