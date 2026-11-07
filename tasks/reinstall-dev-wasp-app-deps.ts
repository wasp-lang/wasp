#!/usr/bin/env node
//MISE description="Reinstalls the current Wasp app's npm dependencies against the dev Wasp npm packages, after dropping their stale lockfile entries. Run it from a Wasp app directory."
//MISE dir="{{cwd}}"

import { reinstallDevWaspAppDeps } from "../waspc/tools/reinstallDevWaspAppDeps.ts";

await reinstallDevWaspAppDeps();
