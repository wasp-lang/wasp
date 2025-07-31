import { existsSync } from "fs";
import { dirname, join } from "path";

export function findNodeProjectRootDirectory(
  startingDirectory: string,
): string {
  let currentDirectory = startingDirectory;

  while (currentDirectory !== dirname(currentDirectory)) {
    if (existsSync(join(currentDirectory, "package.json"))) {
      return currentDirectory;
    }
    currentDirectory = dirname(currentDirectory);
  }

  throw new Error("Could not find project root directory");
}
