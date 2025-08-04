import path from "path";

export function getFileNameWithoutExtension(filePath: string): string {
  return path.basename(filePath, path.extname(filePath));
}
