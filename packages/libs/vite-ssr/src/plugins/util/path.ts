import * as fs from "node:fs/promises";

export function removeLeadingSlash(path: string) {
  return path.replace(/^\//, "");
}

export async function pathExists(...params: Parameters<typeof fs.access>) {
  try {
    await fs.access(...params);
    return true;
  } catch {
    return false;
  }
}
