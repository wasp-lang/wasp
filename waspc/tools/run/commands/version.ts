/// <reference types="node" />

import { getWaspcVersion } from "../../get-waspc-version.ts";
import type { Command } from "../command.ts";
import { runNodeTool } from "../workspace.ts";

export const versionCommands: Command[] = [
  {
    name: "get-waspc-version",
    description: "Gets the current version of waspc from the Haskell project.",
    run: async () => {
      process.stdout.write(getWaspcVersion());
    },
  },
  {
    name: "version-bump",
    argsHint: "<major | minor | patch>",
    description:
      "Bumps the Wasp version across waspc.cabal, libs, and example projects, then rebuilds libs and busts each project's libs cache.",
    run: (args) => runNodeTool("version-bump.ts", args),
  },
];
