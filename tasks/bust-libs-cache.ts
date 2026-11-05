#!/usr/bin/env node
//MISE description="Busts the npm cache for all @wasp/lib-* packages in the current Wasp app. Must be run from a Wasp app directory."
//MISE dir="{{cwd}}"

import { execa } from "execa";
import { rm } from "node:fs/promises";
import { join } from "node:path";
import { reinstallDevWaspAppDeps } from "../waspc/tools/reinstallDevWaspAppDeps.ts";
import { getWaspcDirPath } from "../waspc/tools/utils.ts";
import { GENERATED_WASP_PATHS } from "../waspc/tools/waspAppPackageLock.ts";

const waspCliDevPath = join(getWaspcDirPath(), "tools", "wasp-cli-dev");
const $ = execa({ stdio: "inherit", verbose: "short" });

// Old generated workspaces point at old lib tarballs, so we drop them first.
// Installing before compiling makes the installed @wasp/wasp match the CLI.
for (const generatedPath of GENERATED_WASP_PATHS) {
  await rm(generatedPath, { recursive: true, force: true });
}
await reinstallDevWaspAppDeps();

// Compiling the project copies the new lib tarball to the out dir.
await $`${waspCliDevPath} compile`;

// The --force flag tells `npm` to ignore local cache when installing packages.
await $`npm install --force`;
