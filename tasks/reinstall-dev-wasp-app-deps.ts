#!/usr/bin/env node
//MISE description="Reinstalls the current Wasp app's npm dependencies against the dev Wasp npm packages, after dropping their stale lockfile entries. Run it from a Wasp app directory."
//MISE dir="{{cwd}}"

import { execa } from "execa";
import { removeStaleGeneratedWaspLockEntries } from "../waspc/tools/waspAppPackageLock.ts";

const $ = execa({ stdio: "inherit", verbose: "short" });

// The dev Wasp npm packages are `file:` tarballs that change whenever their
// sources change, and then the lockfile's entries for them go stale.
await removeStaleGeneratedWaspLockEntries();
await $`npm install`;
