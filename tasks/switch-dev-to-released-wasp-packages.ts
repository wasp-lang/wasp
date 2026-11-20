#!/usr/bin/env node
//MISE description="Switches the current Wasp app from the Wasp in this repo to a released Wasp version, and reinstalls its dependencies. Run it from a Wasp app directory."
//MISE dir="{{cwd}}"
//USAGE arg "<version>"

import { execa } from "execa";
import { WASP_MAIN_PACKAGE_NAME } from "../waspc/tools/packages/utils.ts";

const [_node, _filename, version] = process.argv;
if (!version) {
  throw new Error("Pass the Wasp version to switch to, e.g. 0.27.0.");
}

// The repo's Wasp apps depend on `@wasp/wasp` through `workspace:*`. A plain
// version makes pnpm install the released package from npm instead.
const $ = execa({ stdio: "inherit", verbose: "short" });
await $`pnpm pkg set dependencies.${WASP_MAIN_PACKAGE_NAME}=${version}`;
// The lockfile changes, which CI would refuse by default.
await $`pnpm install --no-frozen-lockfile`;
