/// <reference types="node" />

import { ExecaError, execa } from "execa";
import {
  copyFileSync,
  existsSync,
  globSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { join } from "node:path";
import { styleText } from "node:util";
import { getWaspcVersion } from "../get-waspc-version.ts";
import { getRepoRootPath, getWaspcDirPath } from "../utils.ts";
import { runCmd } from "./runCmd.ts";

export type Command = {
  name: string;
  argsHint?: string;
  description: string;
  run: (args: string[]) => Promise<void>;
};

const waspcDir = getWaspcDirPath();
const repoRootDir = getRepoRootPath();

// A single executable that runs the locally-built CLI via `cabal run`.
// Being a single executable (rather than a multi-word command string) lets it
// be a drop-in replacement for `wasp`, with no shell-quoting surprises.
const waspCliDevPath = join(waspcDir, "tools", "wasp-cli-dev");
const waspCliDevEnv = { WASP_CLI_CMD: waspCliDevPath };

// Wasp App Runner
// Utility used for running e2e tests in Wasp apps, we use it for testing
// e.g. examples and starters.
// By passing this, we are telling all of our e2e tests to use the local
// version of Wasp App Runner, built by `buildWaspAppRunner` (tests should
// make sure to call that one before).
const waspAppRunnerCliPath = join(
  repoRootDir,
  "packages/wasp-app-runner/bin/index.js",
);
const e2eTestsEnv = {
  ...waspCliDevEnv,
  WASP_APP_RUNNER_CLI_CMD: waspAppRunnerCliPath,
};

const KITCHEN_SINK_PATH = "examples/kitchen-sink";
const EXAMPLE_APP_PATHS = [
  "examples/tutorials/TodoApp",
  "examples/tutorials/TodoAppTs",
  "examples/waspello",
  "examples/waspleau",
  "examples/websockets-realtime-voting",
  "examples/ask-the-documents",
  KITCHEN_SINK_PATH,
];

export const commandGroups: Command[][] = [
  [
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
        await buildPackagesAndLibs();
        await runInWaspc("cabal", ["install", "--overwrite-policy=always"]);
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
      run: async () => {
        deleteGoldenSnapshots();
        await testWaspcE2e();
      },
    },
    {
      name: "test:cli",
      description:
        "Executes only cli tests. Builds the project first if needed.",
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
    {
      name: "test:kitchen-sink",
      description:
        "Executes only kitchen-sink e2e tests. Installs deps first if needed.",
      run: async () => {
        await buildWaspAppRunner();
        await testExampleApp(KITCHEN_SINK_PATH);
      },
    },
    {
      name: "test:examples",
      description:
        "Executes only examples e2e tests. Installs deps first if needed.",
      run: testExampleApps,
    },
    {
      name: "test:starters",
      description:
        "Executes only starter templates e2e tests. Installs deps first if needed.",
      run: testStarters,
    },
  ],
  [
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
        await runOrmolu("inplace");
        await runCabalGild("format");
        await runPrettier("format");
      },
    },
    {
      name: "check",
      description:
        "Runs all code formatters in check mode and reports if code is correctly formatted or not.",
      run: async () => {
        await runOrmolu("check");
        await runCabalGild("check");
        await runPrettier("check");
      },
    },
    {
      name: "format:ormolu",
      description:
        "Runs the Haskell code formatter and formats the code in place.",
      run: () => runOrmolu("inplace"),
    },
    {
      name: "check:ormolu",
      description:
        "Runs the Haskell code formatter and reports if code is correctly formatted or not.",
      run: () => runOrmolu("check"),
    },
    {
      name: "format:cabal",
      description:
        "Runs the .cabal file formatter and formats the code in place.",
      run: () => runCabalGild("format"),
    },
    {
      name: "check:cabal",
      description:
        "Runs the .cabal file formatter and reports if code is correctly formatted or not.",
      run: () => runCabalGild("check"),
    },
    {
      name: "format:prettier",
      description:
        "Runs the repo-wide code formatter and formats the code in place.",
      run: () => runPrettier("format"),
    },
    {
      name: "check:prettier",
      description:
        "Runs the repo-wide code formatter and reports if code is correctly formatted or not.",
      run: () => runPrettier("check"),
    },
  ],
  [
    {
      name: "get-waspc-version",
      description:
        "Gets the current version of waspc from the Haskell project.",
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
  ],
];

export function findCommand(name: string): Command | undefined {
  return commandGroups.flat().find((command) => command.name === name);
}

async function runInWaspc(
  cmd: string,
  args: string[],
  env?: Record<string, string>,
): Promise<void> {
  await runCmd(cmd, args, { cwd: waspcDir, env });
}

async function runInRepoRoot(cmd: string, args: string[]): Promise<void> {
  await runCmd(cmd, args, { cwd: repoRootDir });
}

async function runWaspCli(args: string[], appDir: string): Promise<void> {
  await runCmd(waspCliDevPath, args, { cwd: appDir });
}

// The tools and prettier are workspace projects, so install before using them.
async function installWorkspace(): Promise<void> {
  await runInRepoRoot("pnpm", ["install"]);
}

async function runNodeTool(
  toolPath: string,
  toolArgs: string[] = [],
): Promise<void> {
  await installWorkspace();
  await runInWaspc("node", [`tools/${toolPath}`, ...toolArgs]);
}

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

async function buildWaspAppRunner(): Promise<void> {
  await installWorkspace();
  await runInRepoRoot("pnpm", [
    "--filter",
    "@wasp/wasp-app-runner",
    "run",
    "build",
  ]);
}

async function testExampleApps(): Promise<void> {
  await buildWaspAppRunner();
  for (const exampleAppPath of EXAMPLE_APP_PATHS) {
    try {
      await testExampleApp(exampleAppPath);
    } catch (error) {
      const failureMessage = `E2E tests failed in ${exampleAppPath}`;
      console.error(
        styleText("red", failureMessage, { stream: process.stderr }),
      );
      throw error;
    }
  }
}

async function testExampleApp(exampleAppPath: string): Promise<void> {
  const exampleAppDir = join(repoRootDir, exampleAppPath);
  copyEnvServerExampleIfNeeded(exampleAppDir);
  await runWaspCli(["install"], exampleAppDir);
  await runCmd("npm", ["run", "test"], {
    cwd: exampleAppDir,
    env: e2eTestsEnv,
  });
}

// Copies .env.server.example to .env.server in the app directory,
// but only if the example exists and .env.server isn't already present.
function copyEnvServerExampleIfNeeded(appDir: string): void {
  const envServerExamplePath = join(appDir, ".env.server.example");
  const envServerPath = join(appDir, ".env.server");
  if (existsSync(envServerExamplePath) && !existsSync(envServerPath)) {
    copyFileSync(envServerExamplePath, envServerPath);
  }
}

async function testStarters(): Promise<void> {
  await buildWaspAppRunner();
  await runCmd("pnpm", ["run", "test:dev"], {
    cwd: join(waspcDir, "starters-e2e-tests"),
    env: e2eTestsEnv,
  });
}

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

async function runStan(stanArgs: string[]): Promise<void> {
  await runInWaspc("cabal", [
    "build",
    "all",
    "--enable-tests",
    "--enable-benchmarks",
  ]);
  await runInWaspc("stan", ["report", ...stanArgs]);
}

async function runHlint(hlintArgs: string[]): Promise<void> {
  await runInWaspc("hlint", [".", ...hlintArgs]);
}

async function runOrmolu(mode: "check" | "inplace"): Promise<void> {
  const haskellFiles = await listTrackedFiles(["*.hs", "*.hs-boot"]);
  await runInWaspc("ormolu", [
    "--color",
    "always",
    "--check-idempotence",
    "--mode",
    mode,
    ...haskellFiles,
  ]);
}

async function runCabalGild(mode: "check" | "format"): Promise<void> {
  const cabalFiles = await listTrackedFiles(["*.cabal"]);
  await runInWaspc("cabal-gild", ["--mode", mode, ...cabalFiles]);
}

async function listTrackedFiles(pathspecs: string[]): Promise<string[]> {
  const { stdout } = await execa("git", ["ls-files", "-z", ...pathspecs], {
    cwd: waspcDir,
  });
  return stdout.split("\0").filter(Boolean);
}

async function runPrettier(mode: "check" | "format"): Promise<void> {
  await installWorkspace();
  await runInRepoRoot("pnpm", ["run", `${mode}:prettier`]);
}

async function runCodeCheck(): Promise<void> {
  const checks: [string, () => Promise<void>][] = [
    ["Formatter (prettier)", () => runPrettier("check")],
    ["Formatter (ormolu)", () => runOrmolu("check")],
    ["Formatter (cabal-gild)", () => runCabalGild("check")],
    ["Linter (hlint)", () => runHlint([])],
    ["Static analysis (stan)", () => runStan([])],
  ];

  const summaryLines: string[] = [];
  let allPassed = true;
  for (const [checkName, check] of checks) {
    const passed = await passes(check);
    allPassed &&= passed;
    summaryLines.push(`${checkName}: ${formatCheckStatus(passed)}`);
  }

  console.log("\n");
  console.log("======================================");
  console.log("               SUMMARY");
  console.log("======================================");
  console.log();
  console.log(summaryLines.join("\n"));
  console.log("-----------------------");
  console.log(`All together: ${formatCheckStatus(allPassed)}`);
  if (!allPassed) {
    process.exitCode = 1;
  }
}

// Only a process that ran and exited non-zero counts as a failed check.
// Interrupts and missing tools still stop the whole code check.
async function passes(check: () => Promise<void>): Promise<boolean> {
  try {
    await check();
    return true;
  } catch (error) {
    if (error instanceof ExecaError && error.exitCode !== undefined) {
      return false;
    }
    throw error;
  }
}

function formatCheckStatus(passed: boolean): string {
  return passed ? styleText("green", "OK") : styleText("red", "FAIL");
}

const GENERATED_WASP_PATHS = [
  ".wasp/out",
  "node_modules/@wasp/generated-server",
  "node_modules/@wasp.sh/generated-server",
  "node_modules/wasp",
];

async function bustLibsCache(appDir: string): Promise<void> {
  // `wasp install` must run before compile so @wasp/spec matches CLI version.
  // Old generated workspaces point at old lib tarballs, so remove them from disk
  // and lockfile first. Compile then regenerates .wasp/out with fresh tarballs.
  for (const generatedPath of GENERATED_WASP_PATHS) {
    rmSync(join(appDir, generatedPath), { recursive: true, force: true });
  }
  removeStaleGeneratedWaspLockEntries(appDir);
  await runWaspCli(["install"], appDir);

  // Compiling the project copies the new lib tarball to the out dir.
  await runWaspCli(["compile"], appDir);

  // The --force flag tells `npm` to ignore local cache when installing packages.
  await runCmd("npm", ["install", "--force"], { cwd: appDir });
}

type PackageLockEntry = { dependencies?: Record<string, string> };

// We remove .wasp/out before `wasp install`, so package-lock entries for
// generated workspaces and their local tarball deps become stale. Drop them so
// npm rebuilds those entries from the freshly generated package.json files.
// Apps generated before Wasp 0.27 used the `@wasp.sh` npm scope instead of
// `@wasp`, so we match both.
function removeStaleGeneratedWaspLockEntries(appDir: string): void {
  const packageLockPath = join(appDir, "package-lock.json");
  const packageLock: { packages: Record<string, PackageLockEntry> } =
    JSON.parse(readFileSync(packageLockPath, "utf-8"));
  const freshLockEntries = Object.entries(packageLock.packages)
    .filter(([packagePath]) => !isStaleGeneratedWaspPackage(packagePath))
    .map(([packagePath, lockEntry]) => [
      packagePath,
      withoutWaspLibDependencies(lockEntry),
    ]);

  packageLock.packages = Object.fromEntries(freshLockEntries);
  writeFileSync(packageLockPath, `${JSON.stringify(packageLock, null, 2)}\n`);
}

function isStaleGeneratedWaspPackage(packagePath: string): boolean {
  const isWaspLib =
    packagePath.startsWith("node_modules/") &&
    isWaspLibName(packagePath.slice("node_modules/".length));
  const isGenerated = GENERATED_WASP_PATHS.some(
    (generatedPath) =>
      packagePath === generatedPath ||
      packagePath.startsWith(`${generatedPath}/`),
  );
  return isWaspLib || isGenerated;
}

function withoutWaspLibDependencies(
  lockEntry: PackageLockEntry,
): PackageLockEntry {
  if (!lockEntry.dependencies) {
    return lockEntry;
  }
  const dependencies = Object.fromEntries(
    Object.entries(lockEntry.dependencies).filter(
      ([dependencyName]) => !isWaspLibName(dependencyName),
    ),
  );
  return { ...lockEntry, dependencies };
}

function isWaspLibName(packageName: string): boolean {
  return /^@wasp(\.sh)?\/lib-/.test(packageName);
}
