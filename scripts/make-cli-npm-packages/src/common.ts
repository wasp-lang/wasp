import * as fs from "node:fs";
import * as path from "node:path";

export const TEMPLATES_DIR = path.resolve(import.meta.dirname, "../templates");

// The CLI package (`@wasp/cli`) comes from the `packages/cli` workspace
// package. The published package gets these files unchanged, and a
// `package.json` generated from the source one.
export const CLI_PACKAGE_SOURCE_DIR = path.resolve(
  import.meta.dirname,
  "../../../packages/cli",
);
export const CLI_PACKAGE_STATIC_FILES = [
  "bin.js",
  "CLIError.js",
  "fs-utils.js",
  "node-resolution.js",
  "project-wasp-cli.js",
  "run.js",
  "readme.md",
];

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
