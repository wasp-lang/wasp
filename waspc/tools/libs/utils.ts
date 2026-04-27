import { join } from "node:path";
import { getWaspcDirPath } from "../utils";

export function getDataLibsDirPath(): string {
  const waspcDirPath = getWaspcDirPath();
  return join(waspcDirPath, "data", "Generator", "libs");
}
