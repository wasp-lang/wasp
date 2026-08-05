import * as fs from "node:fs";
import * as path from "node:path";

export const TEMPLATES_DIR = path.resolve(import.meta.dirname, "../templates");

export const RUNNABLE_PACKAGE_NAMES = [
  "@wasp.sh/internal-deploy",
  "@wasp.sh/internal-ts-inspect",
  "@wasp.sh/internal-prisma",
  "@wasp.sh/internal-studio-server",
  "@wasp.sh/spec",
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
