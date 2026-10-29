import * as fs from "fs";

export function doesFileExist(filePath: string): boolean {
  const stats = fs.statSync(filePath, { throwIfNoEntry: false });
  return stats !== undefined && stats.isFile();
}
