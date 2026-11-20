import * as fs from "node:fs/promises";
import * as path from "node:path";

// The compiled code runs from `dist/`, one level below the package root.
const packageRootDir = path.join(import.meta.dirname, "..");

export async function readJsonFromPackageRoot<T>(fileName: string): Promise<T> {
  const filePath = path.join(packageRootDir, fileName);
  return JSON.parse(await fs.readFile(filePath, "utf8")) as T;
}
