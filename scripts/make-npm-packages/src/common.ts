import * as fs from "node:fs";
import * as path from "node:path";

export const TEMPLATES_DIR = path.resolve(import.meta.dirname, "../templates");

// The main package (`@wasp/cli`) is a workspace package, this script turns it
// into the published one: it adds the version and the platform sub-packages.
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
