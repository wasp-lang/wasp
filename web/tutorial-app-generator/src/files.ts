import fs from "fs/promises";
import path from "path";

export async function ensureDirExists(dirPath: string): Promise<void> {
  await fs.mkdir(dirPath, { recursive: true });
}

export async function doesFileExist(filePath: string): Promise<boolean> {
  try {
    await fs.access(filePath);
    return true;
  } catch (error) {
    return false;
  }
}

export function getFileNameWithoutExtension(filePath: string): string {
  return path.basename(filePath, path.extname(filePath));
}
