import * as fs from "node:fs";
import * as path from "node:path";

export const TEMPLATES_DIR = path.resolve(import.meta.dirname, "../templates");

export function copyStaticFiles(
  fileNames: string[],
  sourceDir: string,
  destDir: string,
) {
  console.group("Copying static files...");

  for (const fileName of fileNames) {
    fs.cpSync(path.join(sourceDir, fileName), path.join(destDir, fileName), {
      recursive: true,
    });
    console.log(`Copied ${fileName}`);
  }

  console.groupEnd();
}
