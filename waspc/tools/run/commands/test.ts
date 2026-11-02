/// <reference types="node" />

import { globSync, rmSync } from "node:fs";
import { join } from "node:path";
import type { Command } from "../command.ts";
import { runInWaspc, waspcDir } from "../runCmd.ts";
import { waspCliDevEnv } from "../waspCliDev.ts";
import { runNodeTool } from "../workspace.ts";
import { testExampleApps, testStarters } from "./appE2eTests.ts";

export const testCommands: Command[] = [
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
    run: async () => {
      deleteGoldenSnapshots();
      await testWaspcE2e();
    },
  },
  {
    name: "test:cli",
    description: "Executes only cli tests. Builds the project first if needed.",
    run: () => runCabalTestSuite("wasp-cli-tests"),
  },
  {
    name: "test:libs",
    description: "Run tests on Wasp libs (types, unit, type-exports).",
    run: () => runNodeTool("libs/test.ts"),
  },
  {
    name: "test:packages",
    description: "Run tests on Wasp packages (spec, deploy, etc.).",
    run: () => runNodeTool("packages/test.ts"),
  },
];

async function runAllTests(): Promise<void> {
  try {
    await runInWaspc("cabal", ["test"], waspCliDevEnv);
    console.log("Running e2e tests");
    await testStarters();
    await testExampleApps();
    await runNodeTool("libs/test.ts");
    await runNodeTool("packages/test.ts");
    console.log("ALL TESTS PASSED");
  } catch (error) {
    console.log("SOME TESTS FAILED");
    throw error;
  }
}

async function runCabalTestSuite(
  testSuite: string,
  testOptions: string[] = [],
): Promise<void> {
  await runInWaspc(
    "cabal",
    ["test", testSuite, "--test-option=--hide-successes", ...testOptions],
    waspCliDevEnv,
  );
}

async function testWaspcUnit(testPattern?: string): Promise<void> {
  const patternOptions = testPattern
    ? ["--test-option=-p", `--test-option=${testPattern}`]
    : [];
  await runCabalTestSuite("waspc-tests", patternOptions);
}

async function testWaspcE2e(): Promise<void> {
  await runCabalTestSuite("waspc-e2e-tests");
}

function deleteGoldenSnapshots(): void {
  const goldenSnapshotDirs = globSync(
    "e2e-tests/test-outputs/snapshots/*-golden",
    { cwd: waspcDir },
  );
  for (const goldenSnapshotDir of goldenSnapshotDirs) {
    rmSync(join(waspcDir, goldenSnapshotDir), { recursive: true });
  }
}
