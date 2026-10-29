import fs from "fs";
import path from "node:path";

export function assertDirPathIsAbsolute(
  dirPath: string,
  dirNameInError: string,
): void {
  if (!path.isAbsolute(dirPath)) {
    throw new Error(`The ${dirNameInError} path must be absolute.`);
  }
}

export function assertDirExists(dirPath: string, dirNameInError: string): void {
  const dirExists = fs.existsSync(dirPath);
  if (!dirExists) {
    throw new Error(`The ${dirNameInError} does not exist.`);
  }
}
