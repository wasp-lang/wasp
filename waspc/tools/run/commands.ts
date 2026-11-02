/// <reference types="node" />

import { bustLibsCache } from "./tasks/bustLibsCache.ts";
import { runCodeCheck } from "./tasks/codeCheck.ts";
import {
  buildWaspAppRunner,
  testExampleApps,
  testKitchenSink,
  testStarters,
} from "./tasks/e2eTests.ts";
import {
  checkCabalGild,
  checkOrmolu,
  checkPrettier,
  formatCabalGild,
  formatOrmolu,
  formatPrettier,
} from "./tasks/formatting.ts";
import {
  acceptAllWaspcE2eSnapshots,
  buildHaskell,
  installWaspCli,
  runGhcid,
  runGhcidOnTests,
  runHlint,
  runStan,
  testAllCabalSuites,
  testWaspCli,
  testWaspcE2e,
  testWaspcUnit,
} from "./tasks/haskell.ts";
import {
  buildLibs,
  buildPackages,
  bumpWaspVersion,
  installWorkspace,
  printWaspcVersion,
  testLibs,
  testPackages,
} from "./tasks/nodeTools.ts";
import { runWaspCli } from "./tasks/waspCli.ts";

export type Command = {
  name: string;
  argsHint?: string;
  description: string;
  run: (args: string[]) => Promise<void>;
};

export const commandGroups: Command[][] = [
  [
    {
      name: "build",
      description:
        "Builds the Haskell project + all sub-projects (i.e. TS packages). Run with the `BUILD_STATIC=1` env to build statically.",
      run: async () => {
        await installWorkspace();
        await buildPackages();
        await buildLibs();
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
      run: async () => {
        await installWorkspace();
        await buildPackages();
      },
    },
    {
      name: "build:libs",
      description:
        "Builds the Wasp libs under packages/libs/ and packs them into data/Generator/libs/.",
      run: async () => {
        await installWorkspace();
        await buildLibs();
      },
    },
  ],
  [
    {
      name: "bust-libs-cache",
      description:
        "Busts the npm cache for all @wasp/lib-* packages in the current Wasp app. Must be run from a Wasp app directory.",
      run: () => bustLibsCache(process.cwd()),
    },
  ],
  [
    {
      name: "wasp-cli",
      argsHint: "<args>",
      description:
        "Runs the dev version of wasp executable in the current directory while forwarding arguments. Builds the project (hs) first if needed.",
      run: (args) => runWaspCli(args, process.cwd()),
    },
  ],
  [
    {
      name: "install",
      description:
        "Installs the wasp executable as 'wasp-cli' globally on the machine.",
      run: async () => {
        await installWorkspace();
        await buildPackages();
        await buildLibs();
        await installWaspCli();
      },
    },
  ],
  [
    {
      name: "test",
      description:
        "Executes all tests (waspc + Wasp CLI + examples + starters). Builds the project first if needed.",
      run: runAllTests,
    },
    {
      name: "test:waspc",
      description:
        "Executes all waspc tests (unit + e2e). Builds the project first if needed.",
      run: async () => {
        await testWaspcUnit();
        await testWaspcE2e();
      },
    },
    {
      name: "test:waspc:unit",
      argsHint: "[pattern]",
      description:
        "Executes only waspc unit tests. Builds the project first if needed. If pattern is provided, it will run only tests whose description/name matches the pattern. Check https://github.com/UnkindPartition/tasty#patterns to learn more about valid patterns.",
      run: ([testPattern]) => testWaspcUnit(testPattern),
    },
    {
      name: "test:waspc:e2e",
      description:
        "Executes only cli e2e tests. Builds the project first if needed.",
      run: testWaspcE2e,
    },
    {
      name: "test:waspc:e2e:accept-all",
      description:
        "Accepts any diffs in the generated app in Wasp CLI snapshot tests. Does so by deleting the current golden output and re-running snapshot tests to produce the new golden output.",
      run: acceptAllWaspcE2eSnapshots,
    },
    {
      name: "test:cli",
      description:
        "Executes only cli tests. Builds the project first if needed.",
      run: testWaspCli,
    },
    {
      name: "test:libs",
      description: "Run tests on Wasp libs (types, unit, type-exports).",
      run: async () => {
        await installWorkspace();
        await testLibs();
      },
    },
    {
      name: "test:packages",
      description: "Run tests on Wasp packages (spec, deploy, etc.).",
      run: async () => {
        await installWorkspace();
        await testPackages();
      },
    },
    {
      name: "test:kitchen-sink",
      description:
        "Executes only kitchen-sink e2e tests. Installs deps first if needed.",
      run: async () => {
        await installWorkspaceAndBuildWaspAppRunner();
        await testKitchenSink();
      },
    },
    {
      name: "test:examples",
      description:
        "Executes only examples e2e tests. Installs deps first if needed.",
      run: async () => {
        await installWorkspaceAndBuildWaspAppRunner();
        await testExampleApps();
      },
    },
    {
      name: "test:starters",
      description:
        "Executes only starter templates e2e tests. Installs deps first if needed.",
      run: async () => {
        await installWorkspaceAndBuildWaspAppRunner();
        await testStarters();
      },
    },
  ],
  [
    {
      name: "ghcid",
      description:
        "Runs ghcid, which watches source file changes and reports errors. Does not watch tests.",
      run: runGhcid,
    },
    {
      name: "ghcid-test",
      description: "Runs ghcid on both source and tests.",
      run: runGhcidOnTests,
    },
  ],
  [
    {
      name: "code-check",
      description:
        "Checks code by running it through formatter, linter and static analysis.",
      run: runCodeCheck,
    },
    {
      name: "stan",
      argsHint: "<args>",
      description:
        "Runs static code analysis on the code, generating stan.html. Builds the project first if needed.",
      run: runStan,
    },
    {
      name: "hlint",
      argsHint: "<args>",
      description: "Runs linter on the codebase.",
      run: runHlint,
    },
    {
      name: "format",
      description:
        "Runs all code formatters (ormolu, cabal-gild, prettier) and formats the code in place.",
      run: async () => {
        await formatOrmolu();
        await formatCabalGild();
        await installWorkspace();
        await formatPrettier();
      },
    },
    {
      name: "check",
      description:
        "Runs all code formatters in check mode and reports if code is correctly formatted or not.",
      run: async () => {
        await checkOrmolu();
        await checkCabalGild();
        await installWorkspace();
        await checkPrettier();
      },
    },
    {
      name: "format:ormolu",
      description:
        "Runs the Haskell code formatter and formats the code in place.",
      run: formatOrmolu,
    },
    {
      name: "check:ormolu",
      description:
        "Runs the Haskell code formatter and reports if code is correctly formatted or not.",
      run: checkOrmolu,
    },
    {
      name: "format:cabal",
      description:
        "Runs the .cabal file formatter and formats the code in place.",
      run: formatCabalGild,
    },
    {
      name: "check:cabal",
      description:
        "Runs the .cabal file formatter and reports if code is correctly formatted or not.",
      run: checkCabalGild,
    },
    {
      name: "format:prettier",
      description:
        "Runs the repo-wide code formatter and formats the code in place.",
      run: async () => {
        await installWorkspace();
        await formatPrettier();
      },
    },
    {
      name: "check:prettier",
      description:
        "Runs the repo-wide code formatter and reports if code is correctly formatted or not.",
      run: async () => {
        await installWorkspace();
        await checkPrettier();
      },
    },
  ],
  [
    {
      name: "get-waspc-version",
      description:
        "Gets the current version of waspc from the Haskell project.",
      run: async () => printWaspcVersion(),
    },
    {
      name: "version-bump",
      argsHint: "<major | minor | patch>",
      description:
        "Bumps the Wasp version across waspc.cabal, libs, and example projects, then rebuilds libs and busts each project's libs cache.",
      run: async (args) => {
        await installWorkspace();
        await bumpWaspVersion(args);
      },
    },
  ],
];

export function findCommand(name: string): Command | undefined {
  return commandGroups.flat().find((command) => command.name === name);
}

async function installWorkspaceAndBuildWaspAppRunner(): Promise<void> {
  await installWorkspace();
  await buildWaspAppRunner();
}

async function runAllTests(): Promise<void> {
  try {
    await testAllCabalSuites();
    console.log("Running e2e tests");
    await installWorkspaceAndBuildWaspAppRunner();
    await testStarters();
    await testExampleApps();
    await testLibs();
    await testPackages();
    console.log("ALL TESTS PASSED");
  } catch (error) {
    console.log("SOME TESTS FAILED");
    throw error;
  }
}
