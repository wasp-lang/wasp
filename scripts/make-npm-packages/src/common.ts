import * as fs from "node:fs";
import * as path from "node:path";

export const TEMPLATES_DIR = path.resolve(import.meta.dirname, "../templates");

// The source of the main package (`@wasp/cli`) is a workspace package, so
// `@wasp/wasp` can depend on it. This script turns it into the published package.
export const MAIN_PACKAGE_SOURCE_DIR = path.resolve(
  import.meta.dirname,
  "../../../packages/cli",
);

export function copyStaticFiles(
  fileNames: string[],
  sourceDir: string,
  destDir: string,
) {
  console.group("Copying static files...");

  for (const fileName of fileNames) {
    fs.copyFileSync(
      path.join(sourceDir, fileName),
      path.join(destDir, fileName),
    );
    console.log(`Copied ${fileName}`);
  }

  console.groupEnd();
}
