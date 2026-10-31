import * as fs from "node:fs";
import * as path from "node:path";

export const TEMPLATES_DIR = path.resolve(import.meta.dirname, "../templates");

// The main package (`@wasp/cli`) is the `packages/cli` workspace package. These
// files go into the published package as they are, the `package.json` is
// generated from the source one.
export const MAIN_PACKAGE_SOURCE_DIR = path.resolve(
  import.meta.dirname,
  "../../../packages/cli",
);
export const MAIN_PACKAGE_STATIC_FILES = ["bin.js", "CLIError.js", "readme.md"];

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
