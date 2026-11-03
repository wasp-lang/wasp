// Checks that every installed package in the given scope dir (e.g.
// `node_modules/@wasp`) contains the files its package.json points at: the
// `files` entries and the `bin`, `main` and `exports` targets. CI runs it on
// the packages installed from pkg.pr.new, to catch a file the packing left out.
//
// Usage: node check-installed-package-files.ts <scope dir>

import { globSync, readdirSync, readFileSync } from "node:fs";
import { join } from "node:path";

type PackageJson = {
  name: string;
  files?: string[];
  bin?: string | Record<string, string>;
  main?: string;
  exports?: unknown;
};

const scopeDir = process.argv[2];
if (scopeDir === undefined) {
  throw new Error("Usage: node check-installed-package-files.ts <scope dir>");
}

let missingFileCount = 0;
for (const packageDirName of readdirSync(scopeDir)) {
  const packageDir = join(scopeDir, packageDirName);
  const packageJson: PackageJson = JSON.parse(
    readFileSync(join(packageDir, "package.json"), "utf8"),
  );

  // `files` entries can be globs, e.g. `dist/**/*`.
  const missingPaths = getReferencedPaths(packageJson).filter(
    (referencedPath) =>
      globSync(referencedPath, { cwd: packageDir }).length === 0,
  );
  for (const missingPath of missingPaths) {
    console.error(`${packageJson.name} is missing ${missingPath}`);
  }
  missingFileCount += missingPaths.length;
  console.log(`Checked ${packageJson.name}`);
}

if (missingFileCount > 0) {
  process.exit(1);
}

function getReferencedPaths(packageJson: PackageJson): string[] {
  return [
    ...(packageJson.files ?? []),
    ...(typeof packageJson.bin === "string"
      ? [packageJson.bin]
      : Object.values(packageJson.bin ?? {})),
    ...(packageJson.main === undefined ? [] : [packageJson.main]),
    ...getExportsTargets(packageJson.exports),
  ];
}

/** Collects the file paths at the leaves of an `exports` field. */
function getExportsTargets(exportsField: unknown): string[] {
  if (typeof exportsField === "string") {
    return [exportsField];
  }
  if (exportsField === null || typeof exportsField !== "object") {
    return [];
  }
  return Object.values(exportsField).flatMap(getExportsTargets);
}
