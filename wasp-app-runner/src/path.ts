import { join } from "path";

// Gets the absolute path to the waspc directory __from the current file__.
export function getWaspcDirAbsPath() {
  const currentDirAbsPath = new URL(".", import.meta.url).pathname;
  return join(currentDirAbsPath, "../../waspc");
}
