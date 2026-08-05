/// <reference types="node" />
// Helper to pack the publishable waspc/packages/* for release. Used by CI.
//
// Usage: node ./waspc/tools/packages/pack.ts <version> <output-dir>
//
// Writes one directory per package into <output-dir>, containing exactly the
// files that would be published. We extract the tarballs back into directories
// because `pkg-pr-new` only accepts package directories, not tarballs.

import { mkdirSync, mkdtempSync, readdirSync } from "node:fs";
import { tmpdir } from "node:os";
import { basename, join } from "node:path";
import { getPackageJson, getWaspcDirPath, runCmd } from "../utils.ts";
import { discoverPackageDirs } from "./utils.ts";

const [publishVersion, outputDirPath] = process.argv.slice(2);

if (!publishVersion || !outputDirPath) {
  console.error("Usage: pack.ts <version> <output-dir>");
  process.exit(1);
}

packPublishedPackages(publishVersion, outputDirPath);

function packPublishedPackages(version: string, outputDir: string): void {
  const waspcDirPath = getWaspcDirPath();
  const packageDirs = discoverPackageDirs();

  // The packages are npm workspaces of `waspc/`, so a single install at the
  // workspace root installs the dependencies of all of them.
  runCmd("npm", ["ci"], { cwd: waspcDirPath, stdio: "inherit" });

  // We build before overriding the version, because the build asserts that the
  // package version matches the one in `waspc.cabal`, which the override
  // (e.g. an RC version) intentionally doesn't.
  for (const packageDir of packageDirs) {
    runCmd("npm", ["run", "build"], { cwd: packageDir, stdio: "inherit" });
  }

  for (const packageDir of packageDirs) {
    const { name: packageName, private: isPrivate } =
      getPackageJson(packageDir);

    if (isPrivate) {
      console.log(`Skipping ${packageName}, it is not published`);
      continue;
    }

    console.log(`Packing ${packageName} (${packageDir})`);

    runCmd("npm", ["pkg", "set", `version=${version}`], { cwd: packageDir });

    // `npm pack` puts only the published files in the tarball.
    const packDirPath = mkdtempSync(join(tmpdir(), "wasp-pack-"));
    runCmd("npm", ["pack", "--pack-destination", packDirPath], {
      cwd: packageDir,
      stdio: "inherit",
    });

    const [tarballName] = readdirSync(packDirPath);
    if (tarballName === undefined) {
      throw new Error(`npm pack produced no tarball for ${packageName}`);
    }

    const packageOutputDirPath = join(outputDir, basename(packageDir));
    mkdirSync(packageOutputDirPath, { recursive: true });
    runCmd(
      "tar",
      [
        "-xzf",
        join(packDirPath, tarballName),
        "-C",
        packageOutputDirPath,
        "--strip-components=1",
      ],
      { stdio: "inherit" },
    );
  }
}
