/// <reference types="node" />

import type { Command } from "../command.ts";
import { runInWaspc } from "../runCmd.ts";
import { runNodeTool } from "../workspace.ts";

export const buildCommands: Command[] = [
  {
    name: "build",
    description:
      "Builds the Haskell project + all sub-projects (i.e. TS packages). Run with the `BUILD_STATIC=1` env to build statically.",
    run: async () => {
      await buildPackagesAndLibs();
      await buildHaskell();
    },
  },
  {
    name: "build:hs",
    description:
      "Builds the Haskell project only. Run with the `BUILD_STATIC=1` env to build statically.",
    run: buildHaskell,
  },
  {
    name: "build:packages",
    description:
      "Builds the TypeScript packages under packages/ that waspc uses, and copies them into data/packages/.",
    run: () => runNodeTool("packages/build.ts"),
  },
  {
    name: "build:libs",
    description:
      "Builds the Wasp libs under packages/libs/ and packs them into data/Generator/libs/.",
    run: () => runNodeTool("libs/build.ts"),
  },
  {
    name: "install",
    description:
      "Installs the wasp executable as 'wasp-cli' globally on the machine.",
    run: async () => {
      await buildPackagesAndLibs();
      await runInWaspc("cabal", ["install", "--overwrite-policy=always"]);
    },
  },
];

async function buildPackagesAndLibs(): Promise<void> {
  await runNodeTool("packages/build.ts");
  await runNodeTool("libs/build.ts");
}

async function buildHaskell(): Promise<void> {
  const staticBuildFlags = process.env.BUILD_STATIC
    ? ["--enable-executable-static"]
    : [];
  await runInWaspc("cabal", ["build", "all", ...staticBuildFlags]);
}
