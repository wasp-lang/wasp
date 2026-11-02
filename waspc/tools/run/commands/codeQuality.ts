/// <reference types="node" />

import { ExecaError, execa } from "execa";
import { styleText } from "node:util";
import type { Command } from "../command.ts";
import { runInRepoRoot, runInWaspc, waspcDir } from "../runCmd.ts";
import { installWorkspace } from "../workspace.ts";

export const codeQualityCommands: Command[] = [
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
];

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
