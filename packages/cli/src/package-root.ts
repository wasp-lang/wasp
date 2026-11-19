import { existsSync } from "node:fs";
import * as fs from "node:fs/promises";
import * as path from "node:path";

// The compiled code runs from `dist/`, one level below the package root.
export const packageRootDir = path.join(import.meta.dirname, "..");

export function doesFileExistInPackageRoot(fileName: string): boolean {
  return existsSync(path.join(packageRootDir, fileName));
}

export async function readJsonFromPackageRoot<T>(fileName: string): Promise<T> {
  const filePath = path.join(packageRootDir, fileName);
  return JSON.parse(await fs.readFile(filePath, "utf8")) as T;
}
