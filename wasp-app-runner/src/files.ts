import * as fs from "node:fs";

export function doesFileExist(filePath: string): boolean {
  const stats = fs.statSync(filePath, { throwIfNoEntry: false });
  return stats !== undefined && stats.isFile();
}
