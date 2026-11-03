#!/usr/bin/env node
//MISE description="Busts the npm cache for all @wasp/lib-* packages in the current Wasp app. Must be run from a Wasp app directory."
//MISE dir="{{cwd}}"

import { execa } from "execa";
import { rm } from "node:fs/promises";
import { join } from "node:path";
import { getWaspcDirPath } from "../waspc/tools/utils.ts";
import {
  GENERATED_WASP_PATHS,
  removeStaleGeneratedWaspLockEntries,
} from "../waspc/tools/waspAppPackageLock.ts";

const waspCliDevPath = join(getWaspcDirPath(), "tools", "wasp-cli-dev");
const $ = execa({ stdio: "inherit", verbose: "short" });

// Install the dependencies before compiling, so the installed @wasp/wasp
// matches the CLI version. Old generated workspaces point at old lib tarballs,
// so remove them from disk and lockfile first. Compile then regenerates
// .wasp/out with fresh tarballs.
for (const generatedPath of GENERATED_WASP_PATHS) {
  await rm(generatedPath, { recursive: true, force: true });
}
await removeStaleGeneratedWaspLockEntries();
await $`npm install`;

// Compiling the project copies the new lib tarball to the out dir.
await $`${waspCliDevPath} compile`;

// The --force flag tells `npm` to ignore local cache when installing packages.
await $`npm install --force`;
