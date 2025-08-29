// @ts-check

import assert from "node:assert/strict";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { fileURLToPath } from "node:url";
import pkg from "../package.json" with { type: "json" };

const resolve = import.meta.resolve;
assert(resolve, "`import.meta.resolve` must be available.");

const dependencies = Object.keys(pkg.dependencies ?? {});
const targetDir = fileURLToPath(new URL("../data/packages/", import.meta.url));

await fs.rm(targetDir, { recursive: true, force: true });

for (const depName of dependencies) {
  const pkgJsonPath = fileURLToPath(
    resolve(path.join(depName, "package.json")),
  );
  const depPath = path.dirname(pkgJsonPath);
  const { default: depPkg } = await import(pkgJsonPath, {
    with: { type: "json" },
  });

  assert(
    depPkg.files,
    `Package ${depName} must have a "files" field in package.json.`,
  );

  const filesToCopy = concatIterables(
    ["package.json"],
    fs.glob(depPkg.files, { cwd: depPath }),
  );

  for await (const file of filesToCopy) {
    const srcPath = path.join(depPath, file);
    const destPath = path.join(targetDir, path.basename(depPath), file);

    await fs.mkdir(path.dirname(destPath), { recursive: true });

    // Copy the file
    await fs.cp(srcPath, destPath, {
      recursive: true,
      mode: fs.constants.COPYFILE_FICLONE, // Use an efficient fs clone if available
    });
  }
}

/**
 * @template T
 * @param {(Iterable<T> | AsyncIterable<T>)[]} iterables
 */
async function* concatIterables(...iterables) {
  for (const iterable of iterables) {
    yield* iterable;
  }
}
