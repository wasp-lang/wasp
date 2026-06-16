/// <reference types="node" />
// Regenerates derived artifacts from the already-bumped source versions: it rebuilds the
// libs/packages (producing tarballs named after the new version) and busts each example
// project's libs cache (refreshing their package-lock.json references).
//
// It contains NO version math: release-please owns the version and bumps the source files
// (waspc.cabal, the package.json/package-lock.json of each lib/package, and the example
// main.wasp.ts files). This script only re-derives the artifacts that release-please cannot
// regenerate itself, so the committed lockfiles match the bumped sources.

import { join } from "node:path";
import {
  findWaspProjectDirsAbsPathInRepo as findWaspProjectDirsPathsInRepo,
  getWaspcDirPath,
  runCmd,
} from "./utils.ts";

const waspcDirPath = getWaspcDirPath();
const waspProjectDirsPaths = findWaspProjectDirsPathsInRepo();

syncDerivedVersions();

function syncDerivedVersions(): void {
  rebuildLibs();
  rebuildPackages();
  bustWaspProjectsLibsCache();
}

function rebuildLibs(): void {
  runCmd("node", [join("tools", "libs", "build.ts")], {
    cwd: waspcDirPath,
    stdio: "inherit",
  });
}

function rebuildPackages(): void {
  runCmd("node", [join("tools", "packages", "build.ts")], {
    cwd: waspcDirPath,
    stdio: "inherit",
  });
}

function bustWaspProjectsLibsCache(): void {
  const runScriptFilePath = join(waspcDirPath, "run");

  for (const waspProjectDirPath of waspProjectDirsPaths) {
    runCmd(runScriptFilePath, ["bust-libs-cache"], {
      cwd: waspProjectDirPath,
      stdio: "inherit",
    });
  }
}
