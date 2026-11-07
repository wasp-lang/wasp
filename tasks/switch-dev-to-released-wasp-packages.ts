#!/usr/bin/env node
//MISE description="Switches the current Wasp app from the dev Wasp npm packages to a released Wasp version, and reinstalls its dependencies. Run it from a Wasp app directory."
//MISE dir="{{cwd}}"
//USAGE arg "<version>"

import { execa } from "execa";
import {
  getPackageName,
  WASP_MAIN_PACKAGE_DIR_NAME,
  WASP_NPM_SUBPACKAGES_DIR_NAMES,
} from "../waspc/tools/packages/utils.ts";
import { reinstallDevWaspAppDeps } from "../waspc/tools/reinstallDevWaspAppDeps.ts";

const [_node, _filename, version] = process.argv;
if (!version) {
  throw new Error("Pass the Wasp version to switch to, e.g. 0.27.0.");
}

const $ = execa({ stdio: "inherit", verbose: "short" });
const devOverrides = WASP_NPM_SUBPACKAGES_DIR_NAMES.map(
  (packageDirName) => `overrides.${getPackageName(packageDirName)}`,
);

await $`npm pkg set dependencies.${getPackageName(WASP_MAIN_PACKAGE_DIR_NAME)}=${version}`;
// `//` holds the examples' note about the dev packages.
await $`npm pkg delete ${devOverrides} //`;
await reinstallDevWaspAppDeps();
