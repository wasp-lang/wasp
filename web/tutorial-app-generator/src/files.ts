import fs from "fs/promises";

export async function ensureDirExists(dir: string): Promise<void> {
  await fs.mkdir(dir, { recursive: true });
}

export async function doesFileExist(filePath: string): Promise<boolean> {
  try {
    await fs.access(filePath);
    return true;
  } catch (error) {
    return false;
  }
}
