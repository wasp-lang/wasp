/// <reference types="node" />

import type { Command } from "../command.ts";
import { runInWaspc } from "../runCmd.ts";

export const ghcidCommands: Command[] = [
  {
    name: "ghcid",
    description:
      "Runs ghcid, which watches source file changes and reports errors. Does not watch tests.",
    run: () => runInWaspc("ghcid", ["--command=cabal", "repl"]),
  },
  {
    name: "ghcid-test",
    description: "Runs ghcid on both source and tests.",
    run: () =>
      runInWaspc("ghcid", [
        // --color always is needed for Tasty to turn on the coloring.
        "-T=:main --color always",
        "--command=cabal",
        "repl",
        "tests/TastyDiscoverDriver.hs",
      ]),
  },
];
