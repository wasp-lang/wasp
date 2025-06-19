import fs from "fs";
import path from "node:path";

export function assertDirIsAbsoluteAndPresent(
  dirPath: string,
  dirNameInError: string,
): void {
  if (!path.isAbsolute(dirPath)) {
    throw new Error(`The supplied ${dirNameInError} path must be absolute.`);
  }

  const dirExists = fs.existsSync(dirPath);
  if (!dirExists) {
    throw new Error(`The ${dirNameInError} does not exist.`);
  }
}
