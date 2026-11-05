import * as fs from "node:fs";
import * as path from "node:path";

export const TEMPLATES_DIR = path.resolve(import.meta.dirname, "../templates");

// The CLI package (`@wasp/cli`) is the workspace package this script lives in.
// The published package gets its build output and readme, and a `package.json`
// generated from the source one.
export const CLI_PACKAGE_SOURCE_DIR = path.resolve(
  import.meta.dirname,
  "../../..",
);
export const CLI_PACKAGE_STATIC_FILES = ["dist", "readme.md"];

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
