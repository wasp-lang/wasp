import { promises as fs } from "fs";

export const appDir = "TodoApp";
export const patchesDir = "patches";

export async function ensureDirExists(dir: string): Promise<void> {
  await fs.mkdir(dir, { recursive: true });
}
