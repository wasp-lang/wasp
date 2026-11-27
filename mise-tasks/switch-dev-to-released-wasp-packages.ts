#!/usr/bin/env node
//MISE description="Switches the current Wasp app from the dev Wasp npm packages to a released Wasp version, and reinstalls its dependencies. Run it from a Wasp app directory."
//MISE dir="{{cwd}}"
//USAGE arg "<version>"

import { execa } from "execa";
import {
  WASP_NPM_SUBPACKAGE_NAMES,
  WASP_PACKAGE_NAMES,
} from "../waspc/tools/packages/utils.ts";
import { reinstallDevWaspAppDeps } from "../waspc/tools/reinstallDevWaspAppDeps.ts";
import { getPackageJson } from "../waspc/tools/utils.ts";

const [_node, _filename, version] = process.argv;
if (!version) {
  throw new Error("Pass the Wasp version to switch to, e.g. 0.27.0.");
}

// TODO(#2262): Once the repo's Wasp apps join the pnpm workspace, there are no
// overrides and no note to delete. This only swaps `workspace:*` for the version.
const $ = execa({ stdio: "inherit", verbose: "short" });
const devOverrides = WASP_NPM_SUBPACKAGE_NAMES.map(
  (packageName) => `overrides.${packageName}`,
);

const packageJson = getPackageJson(process.cwd());
const directWaspDeps = (["dependencies", "devDependencies"] as const).flatMap(
  (section) =>
    WASP_PACKAGE_NAMES.filter(
      (packageName) => packageJson[section]?.[packageName],
    ).map((packageName) => `${section}.${packageName}=${version}`),
);

await $`npm pkg set ${directWaspDeps}`;
// `//` holds the examples' note about the dev packages.
await $`npm pkg delete ${devOverrides} //`;
await reinstallDevWaspAppDeps(process.cwd());
