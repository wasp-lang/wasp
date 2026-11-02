/// <reference types="node" />

import { readdirSync, rmSync } from "node:fs";
import { join } from "node:path";
import { getWaspcDirPath } from "../../utils.ts";
import { runCmd } from "../runCmd.ts";
import { waspCliDevPath } from "./waspCli.ts";

const waspcDir = getWaspcDirPath();
const waspCliEnv = { WASP_CLI_CMD: waspCliDevPath };

export async function buildHaskell(): Promise<void> {
  const staticBuildFlags = process.env.BUILD_STATIC
    ? ["--enable-executable-static"]
    : [];
  await runCabal(["build", "all", ...staticBuildFlags]);
}

export async function installWaspCli(): Promise<void> {
  await runCabal(["install", "--overwrite-policy=always"]);
}

export async function testAllCabalSuites(): Promise<void> {
  await runCabal(["test"], waspCliEnv);
}

export async function testWaspcUnit(testPattern?: string): Promise<void> {
  const patternOptions = testPattern
    ? ["--test-option=-p", `--test-option=${testPattern}`]
    : [];
  await runCabalTestSuite("waspc-tests", { testOptions: patternOptions });
}

export async function testWaspcE2e(): Promise<void> {
  await runCabalTestSuite("waspc-e2e-tests", { env: waspCliEnv });
}

export async function acceptAllWaspcE2eSnapshots(): Promise<void> {
  deleteGoldenSnapshots();
  await testWaspcE2e();
}

export async function testWaspCli(): Promise<void> {
  await runCabalTestSuite("wasp-cli-tests");
}

// The ghcid arguments are the ones the bash script passed: `repl` and the
// test driver path are separate arguments after `--command=cabal`.
export async function runGhcid(): Promise<void> {
  await runCmd("ghcid", ["--command=cabal", "repl"], { cwd: waspcDir });
}

export async function runGhcidOnTests(): Promise<void> {
  await runCmd(
    "ghcid",
    [
      // `--color always` is needed for Tasty to turn on the coloring.
      "-T=:main --color always",
      "--command=cabal",
      "repl",
      "tests/TastyDiscoverDriver.hs",
    ],
    { cwd: waspcDir },
  );
}

export async function runStan(stanArgs: string[]): Promise<void> {
  await runCabal(["build", "all", "--enable-tests", "--enable-benchmarks"]);
  await runCmd("stan", ["report", ...stanArgs], { cwd: waspcDir });
}

export async function runHlint(hlintArgs: string[]): Promise<void> {
  await runCmd("hlint", [".", ...hlintArgs], { cwd: waspcDir });
}

async function runCabalTestSuite(
  testSuite: string,
  {
    testOptions = [],
    env,
  }: { testOptions?: string[]; env?: Record<string, string> } = {},
): Promise<void> {
  await runCabal(
    ["test", testSuite, "--test-option=--hide-successes", ...testOptions],
    env,
  );
}

async function runCabal(
  cabalArgs: string[],
  env?: Record<string, string>,
): Promise<void> {
  await runCmd("cabal", cabalArgs, { cwd: waspcDir, env });
}

function deleteGoldenSnapshots(): void {
  const snapshotsDir = join(waspcDir, "e2e-tests", "test-outputs", "snapshots");
  const goldenSnapshotDirs = readdirSync(snapshotsDir).filter((name) =>
    name.endsWith("-golden"),
  );
  for (const goldenSnapshotDir of goldenSnapshotDirs) {
    rmSync(join(snapshotsDir, goldenSnapshotDir), { recursive: true });
  }
}
