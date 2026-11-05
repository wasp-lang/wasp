import * as fs from "node:fs/promises";

export async function doesPathExist(filePath: string): Promise<boolean> {
  return fs.access(filePath).then(
    () => true,
    () => false,
  );
}
