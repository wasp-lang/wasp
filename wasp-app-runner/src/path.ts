import { join } from "path";

export function getWaspcDirAbsPath() {
  const currentDirAbsPath = new URL(".", import.meta.url).pathname;
  return join(currentDirAbsPath, "../../waspc");
}
